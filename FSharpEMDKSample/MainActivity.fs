namespace FSharpEMDKSample

open System
open System.Collections
open System.IO  // StringReader
open System.Xml // XmlReader

open Android.App
open Android.Content
open Android.OS
open Android.Runtime
open Android.Views
open Android.Widget

open Symbol.XamarinEMDK

type Resources = FSharpEMDKSample.Resource

            
// TODO
// Request an intent back from DW to verify the "Switch Profile" command was succesful
// Create logentry for every DW-related action / event
// Add msecs to log entry timestamp 

// UM AUGUST 2017
// When I try to adorn the new type to statically register it in the manifest, compilation fails with
// an error mentioning need to specify a default constructor
// https://stackoverflow.com/questions/14265864/why-does-broadcastreceiver-need-a-default-constructor
// I register dinamically instead the barcodeBroadcastReceiver activity member in OnCreate and it works
// [<BroadcastReceiver(Enabled = true, Exported = true)>]
type bReceiver (func1: String -> String -> String -> Unit, func2: String-> Unit) = 
   inherit BroadcastReceiver()               
   let mutable decodedSource = ""
   let mutable decodedData = ""
   let mutable decodedLabelType = ""
   let mutable activeProfile = ""
   override this.OnReceive (context, intent) =
      let action = intent.Action
      let b = intent.Extras
      match action with 
      | "com.zebra.dwapiexerciser.ACTION" ->
            do 
                decodedSource <- b.GetString "com.symbol.datawedge.source"
                decodedData <- b.GetString "com.symbol.datawedge.data_string"
                decodedLabelType <- b.GetString "com.symbol.datawedge.label_type"
                func1 decodedSource decodedData decodedLabelType
      | "com.symbol.datawedge.api.RESULT_ACTION" ->
            do 
                activeProfile <- b.GetString "com.symbol.datawedge.api.RESULT_GET_ACTIVE_PROFILE"
                func2 activeProfile
      | _ ->
         ()


[<Activity (Label = "FSharpEMDKSample", MainLauncher = true, Icon = "@mipmap/icon")>]
type MainActivity () =
    inherit Activity ()

    let mutable emdkManager = None
    let mutable profileManager = None
    
    let mutable listViewStatusLog:ListView = null
    let mutable logAdapter:ArrayAdapter = null
    let zebraLog = new JavaList<string> ()

    let resultIsOk (results:EMDKResults) =
        let errorFoundXML statusString =
            let mutable failure = false
            use reader =  XmlReader.Create (new StringReader(statusString))
            while reader.Read() do
                match reader.Name with
                | "parm-eror" -> (failure <- true)
                | "characteristic-error" -> (failure <- true)
                | _ -> ()
            failure    
        results.StatusCode = EMDKResults.STATUS_CODE.Success || ( not (errorFoundXML results.StatusString) )

    let notification = new Event<String> ()
    [<CLIEvent>]
    member this.ZebraNotification = notification.Publish 
           
    member this.showBarcodeToast (a:String) (b:String) (c:String) = 
       this.RunOnUiThread( fun() -> 
          let barcodeToast = (Android.Widget.Toast.MakeText(this, b + "\n" + c, Android.Widget.ToastLength.Long))
          do barcodeToast.Show() )

    member this.showActiveProfile (a:String) = 
       this.RunOnUiThread( fun() -> 
          let barcodeToast = (Android.Widget.Toast.MakeText(this, "Active Profile: " + a, Android.Widget.ToastLength.Long))
          do barcodeToast.Show() )
            
    member this.barcodeBroadcastReceiver = new bReceiver(this.showBarcodeToast, this.showActiveProfile)
    
    member this.ZebraInit () = 
        let results = EMDKManager.GetEMDKManager (Application.Context, this)
        do notification.Trigger ("GetEMDKManager" + (if (results.StatusCode <> EMDKResults.STATUS_CODE.Success) then " KO" else " OK"))

    member this.ZebraSetPowerProfile (resetParam) =
        let profileName = "PowerMgrProfile-1"
        let featureName = "PowerMgr1"
        let modifyData = ProfileManager.CreateNameValuePair(featureName, "ResetAction", resetParam  )  
        do this.ZebraProcessProfile (profileName, [|modifyData|]) |> ignore 

    member this.ZebraProcessProfile (profileName, p2:string[]) =
        let asyncProcessJobAndCheck (pm:ProfileManager) =
            use results = pm.ProcessProfileAsync (profileName, ProfileManager.PROFILE_FLAG.Set, p2)
            do notification.Trigger ("ProcessProfileAsync" + if (results.StatusCode = EMDKResults.STATUS_CODE.Processing) then "OK" else  "KO")
        profileManager |> Option.map asyncProcessJobAndCheck

    member this.Release () =
        do emdkManager |> Option.map (fun (em:EMDKManager) -> em.Release()) |> ignore
        do profileManager <- None
        do emdkManager <- None

    interface EMDKManager.IEMDKListener with        
        member this.OnClosed () =
            match emdkManager with 
            | None -> () 
            | Some (em:EMDKManager) -> do em.Release(); do emdkManager <- None; do profileManager <- None
            notification.Trigger "emdkManager has closed"

        member this.OnOpened emdkManagerInstance = 
            do emdkManager <- Some emdkManagerInstance
            try 
                let pm = emdkManagerInstance.GetInstance (EMDKManager.FEATURE_TYPE.Profile) :?> ProfileManager
                do pm.Data.Subscribe this.ProfileManagerData |> ignore
                do profileManager <- Some pm       
                do notification.Trigger "GetInstance success"
            with | _ -> notification.Trigger "GetInstance failed"
   
    member this.ProfileManagerData (e:ProfileManager.DataEventArgs) =
        notification.Trigger (if resultIsOk (e.P0.Result) then "Profile succesfully applied" else "Profile application failed")

    member this.sendSwitchProfileIntent (profileName:string) =
            let dw = new Intent ()
            // legacy method for SwichtoProfile in DW 6.2
            do  dw.SetAction "com.symbol.datawedge.api.ACTION_SWITCHTOPROFILE" |> ignore
            do  dw.PutExtra("com.symbol.datawedge.api.EXTRA_PROFILENAME", profileName) |> ignore
            do  this.SendBroadcast dw

    member this.getActiveProfileIntent () =
            let dw = new Intent ()
            // legacy method for SwichtoProfile in DW 6.2
            do  dw.SetAction "com.symbol.datawedge.api.ACTION" |> ignore
            do  dw.PutExtra ("com.symbol.datawedge.api.GET_ACTIVE_PROFILE", "") |> ignore
            do  this.SendBroadcast dw

    override this.OnCreateOptionsMenu menu =
        let inflater = new MenuInflater (this) 
        do inflater.Inflate (Resources.Menu.option, menu)
        true

    override this.OnOptionsItemSelected item = 
        // http://techdocs.zebra.com/datawedge/6-5/guide/settings/
        let Asset2DWAutoImport filename =
            let path = "/enterprise/device/settings/datawedge/autoimport/"
            let assets = this.Assets
            let fromStream = assets.Open filename
            // I create the file - RW for owner only, not visibile to DW
            let toFileStream = File.Create (path + filename)
            do fromStream.CopyTo toFileStream
            do toFileStream.Close ()
            do fromStream.Close ()
            // once it is copied, I give RW access to everyone in order for DW to process it and then remove it.  
            let javaFile =  new Java.IO.File (path + filename)
            do javaFile.SetWritable (true,false) |> ignore
            do javaFile.SetReadable (true,false) |> ignore

        do Asset2DWAutoImport 
            (if item.ItemId = Resources.Id.datawedgeRebuild then
               "datawedge.db"
            elif item.ItemId = Resources.Id.inalcaCreate then
               "dwprofile_INALCA-SIMULSCAN.db"
            elif item.ItemId = Resources.Id.fsharpNOILL then
               "dwprofile_F#NOILL.db"
            else // item.ItemId = Resources.Id.fsharpILL then
               "dwprofile_F#ILL.db"
            )
        true
         
    override this.OnResume () =
        base.OnResume ()
        let radioILLON = this.FindViewById<RadioButton>(Resources.Id.barcodeILL)
        do radioILLON.Checked <- true
        do this.sendSwitchProfileIntent "F#ILL"
                   
    override this.OnCreate (bundle) =
        base.OnCreate (bundle)
        do this.RequestedOrientation <- Android.Content.PM.ScreenOrientation.Nosensor        
        do this.SetContentView (Resources.Layout.Portrait)

        let pwrRadioSuspend = this.FindViewById<RadioButton>(Resources.Id.radioSuspend)
        do listViewStatusLog <- this.FindViewById<ListView>(Resources.Id.listViewStatusLog)
        do logAdapter <- new ArrayAdapter<string>(this, Android.Resource.Layout.TestListItem, zebraLog )
        do listViewStatusLog.Adapter <- logAdapter

        // Get our button from the layout resource, and attach an event to it
        let btnSet = this.FindViewById<Button>(Resources.Id.buttonMX)
        let activeProfile = this.FindViewById<Button>(Resources.Id.activeProfile)

        let buttonDWProfile = this.FindViewById<Button>(Resources.Id.buttonSwitchProfile)
        let pwrRadioSuspend = this.FindViewById<RadioButton>(Resources.Id.radioSuspend)
        let radioILLON = this.FindViewById<RadioButton>(Resources.Id.barcodeILL)
        let radioILLOFF = this.FindViewById<RadioButton>(Resources.Id.barcodeNOILL)

        do btnSet.Click.Subscribe (fun args -> this.ZebraSetPowerProfile(if pwrRadioSuspend.Checked then "1" else "4")) |> ignore
        do buttonDWProfile.Click.Subscribe (fun args -> 
            do this.sendSwitchProfileIntent (if radioILLON.Checked then "F#ILL" else if radioILLOFF.Checked then "F#NOILL" else "INALCA-SIMULSCAN" )
             ) |> ignore
        
        do activeProfile.Click.Subscribe (fun args -> do this.getActiveProfileIntent ()) |> ignore

        do this.ZebraNotification   .Subscribe (fun str -> 
            let currentTImeMillis = Java.Lang.JavaSystem.CurrentTimeMillis()
            let dateTimeOffset = DateTimeOffset.FromUnixTimeMilliseconds (currentTImeMillis)
            let dateTime = dateTimeOffset.UtcDateTime

            let epoch2timestamp (millisec:int64) = 
                let startTime = new DateTime (1970,1,1)
                let time = TimeSpan.FromMilliseconds (float millisec) 
                (startTime.Add time).ToLocalTime().ToLongTimeString()

            do zebraLog.Insert (0, ((epoch2timestamp currentTImeMillis ) + "  " + str))
            do this.RunOnUiThread (fun() -> do logAdapter.NotifyDataSetChanged())
            )|> ignore
             
        do this.ZebraInit ()

        // Added August 2017 UM
        let filter = new IntentFilter "com.zebra.dwapiexerciser.ACTION"
        do filter.AddAction "com.symbol.datawedge.api.RESULT_ACTION"
        do filter.AddCategory "android.intent.category.DEFAULT"
        do this.RegisterReceiver (this.barcodeBroadcastReceiver, filter) |> ignore

    override this.OnDestroy() =
        base.OnDestroy()
        do this.Release()

