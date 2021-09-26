(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Gruppo 25: Veronica Zanella
* Main.fs: main code
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.Main

open System
open System.Diagnostics
open Globals
open System.IO.Pipes
open System.IO


// game mode (client)
//
  
let main_game (w, h) =
    use p = new Process ()
    p.StartInfo.UseShellExecute <- true
    p.StartInfo.CreateNoWindow <- false
    p.StartInfo.Arguments <- "1"
    p.StartInfo.FileName <- Process.GetCurrentProcess().MainModule.FileName
    ignore <| p.Start ()

    use client = new NamedPipeClientStream (".", Config.log_pipe_name, PipeDirection.Out)
    client.Connect ()
    Log <- new remote_logger (client)

    // TODO: call your own main here
    Game.main (w, h)
  
    0
   
    
let main_menu () =
    use p = new Process ()
    p.StartInfo.UseShellExecute <- true
    p.StartInfo.CreateNoWindow <- false
    p.StartInfo.Arguments <- "1"
    p.StartInfo.FileName <- Process.GetCurrentProcess().MainModule.FileName
    ignore <| p.Start ()

    use client = new NamedPipeClientStream (".", Config.log_pipe_name, PipeDirection.Out)
    client.Connect ()
    Log <- new remote_logger (client)

    // TODO: call your own main here
    //Game.main ()
    
  
    //if k.KeyChar = 'q' then Game.start_menu()
    
    
    let quit = Game.start_menu ()
    //Game.main ()


    //if i = 1 then 1 else 0
    quit
    //0
    

// log mode (server)
//

let main_log_server () =
    Log.msg "log server process started"
    Console.Title <- Config.log_console_title
    let server = new NamedPipeServerStream (Config.log_pipe_name, PipeDirection.In)
    Log.msg "waiting for incoming connection..."
    server.WaitForConnection ()
    Log.msg "client connected"
    use r = new StreamReader (server)
    while not r.EndOfStream do
        try
            let fg = r.ReadLine ()
            let parse c = Enum.Parse (typeof<Color>, c) :?> Color
            let s = r.ReadLine().Replace (Config.log_pipe_translate_eol, '\n')
            Console.ForegroundColor <- parse fg
            Console.WriteLine s
            Console.ResetColor ()
        with e -> Log.error "exception caught:%s\nstack trace: %O" e.Message e

    Log.warn "EOF reached: quitting."
    0
// main 
//

[<EntryPoint>]



let main argv = 
    #if TEST
    Test.main ()
    printfn "\nPress any key to quit..."
    Console.ReadKey () |> ignore
    0
    #else
    
   
    
    if argv.Length > 0 then 
        //printfn "%A" argv
        //printfn "\nProva..."
        main_log_server ()
    else  
        // ping-pong from main game to start menu 'til exit from start menu
        let rec aux () =
            let menu, (w, h) = main_menu ()
            if menu = 2 
            then let game = main_game (w, h)
                 if game = 0 then aux ()
                 else game
            else menu
        in aux ()
    
    
   #endif
    
