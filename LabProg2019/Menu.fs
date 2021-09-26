(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Menu.fs: menu
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.Menu

open External
open Gfx
open Maze

type CharInfo  with

    /// Shortcut for creating an option pixel.
    static member menu_option (fg : Color, op: Color, ?bg : Color) = 
        //let bg = Color.Black
        let shallpass = Color.White
        let bg = fg
        pixel.create (Config.filled_pixel_char, fg, bg, shallpass, op)

    /// Check whether this pixel is a menu pixel.
   // member this.isOption = this = pixel.menu_option ()

/// Color-op and option couple. The op is a pixel parameter dedicated to menu option which allow to know the function of the option. Nothing to do with graphix
type coopt = Coopt of string*Color  

/// Couple of menu option and its backgroud color
type bgopt = Bgopt of string*Color   

/// Type menu : manage all the menu in the game.
/// Parameters : width, height, pixel, scene: menu type (pause, win, start), relative position in the window
type menu (w : int, h : int, px : pixel, scene: string, position: int, ?bpx : pixel) as this =
    inherit image (w, h, pixel.empty)
   
    let main_list = ["Size"; "Help"; "Exit"]
    let pause_list = ["Resume"; "Restart"; "New"; "Solution"; "Exit"]
    let win_list = ["New"; "Exit"]
    //let help_list = ["Back"]
    // TODO: generate menu
    do this.generate (scene)
    member private __.menu l t_menu =

        // list color option
        let colist: coopt list = 
            if t_menu = "size" then [Coopt("Small", Color.Green); Coopt("Medium", Color.Green); Coopt("Large", Color.Green);  Coopt("ExtraLarge", Color.Green); ]
            else if t_menu = "help" then [Coopt("Back", Color.DarkRed)]
            else [Coopt("New", Color.DarkGreen); Coopt("Size", Color.Yellow); Coopt("Help", Color.DarkMagenta); Coopt("Start", Color.Green); Coopt("Resume", Color.Blue); Coopt("Restart", Color.DarkBlue); Coopt("Solution", Color.Cyan); Coopt("Exit", Color.Red)]       
        // list background color
        let bg_list = 
            [Bgopt("Size", Color.Blue); Bgopt("Help", Color.DarkGreen); Bgopt("Exit", Color.DarkRed); 
            Bgopt("Small", Color.Cyan); Bgopt("Medium", Color.DarkCyan); Bgopt("Large", Color.Blue); Bgopt("ExtraLarge", Color.DarkBlue);
            Bgopt("Resume", Color.Blue); Bgopt("Restart", Color.Blue); Bgopt("New", Color.Blue); Bgopt("Solution", Color.Blue); Bgopt("Exit", Color.DarkRed);
            Bgopt("Back", Color.DarkRed)]
       
       // Find an option and return its color which correspond to an action
        let rec menu_color co_l op =
            match co_l with
            | [] -> Color.Black
            | Coopt (s, c) :: xs -> if s = op then c
                                    else menu_color xs op 
        // Find an option and return its bakcground color
        let rec menu_bg_color co_l op =
            match co_l with
            | [] -> px.fg
            | Bgopt (s, c) :: xs -> if s = op then c
                                    else menu_bg_color xs op 
        
        
        let mutable y0 = 3
        let x0 = position
        // rectangle width and height 
        let w = if t_menu <> "pause" then 40 else 20
        let h = 5
        // x coordinate
        let x1 = x0 + w - 1
        // for iteration in the list passed in current method
        for i in l do 
            // get color op for the i element
            let color_option = menu_color colist i
            // get fg color from menu px passed in current method
            let mfg = menu_bg_color bg_list i
            // set menu pixel
            let mpx = pixel.menu_option (mfg, color_option)
            this.draw_rectangle (x0, y0, w, h, mpx)  
            //this.draw_text(i, (x0 + 2), (y0 + 2), Color.Black)
            for j in 0..h do
                this.draw_line(x0, (y0+j), x1, (y0+j), mpx)
            this.draw_text(i, (x0 + 4), (y0 + 2), Color.Yellow, mfg)   
            y0 <- y0 + 5
        if t_menu = "help" then
            this.draw_rectangle (x0, y0, w, 15, pixel.filled Color.Green)
            let instructions: string = "\n w, a, s, d : up, left, down, right \n m : show / hide menu \n n : new maze \n p : show / hide path \n q : quit game  "
            
            this.draw_text(instructions, (x0 + 2), (y0 + 2), Color.Green)

    //Different type of menu: pause, win, start
    /// generate pause menu
    member private __.pause_menu =
        let option_list = pause_list
        this.menu option_list "pause"
    member private __.win_menu =
           let option_list = win_list
           this.menu option_list "win"
    /// generate start menu
    member private __.main_menu =
           let option_list = main_list
           this.menu option_list "main"
    // on-click functions
    /// Size menu
    member private __.size =
        let option_list = ["Small"; "Medium"; "Large"; "ExtraLarge"]
        this.menu option_list "size"
    /// Help menu
    member private __.help =
        let option_list = ["Back"]
        this.menu option_list "help"
    /// main function to genwrate menu
    member private __.generate (scene) =  
        match scene with
        | "start"   -> this.main_menu
        | "size"    -> this.size
        | "win"     -> this.win_menu
        | "help"    -> this.help
        | _ -> this.pause_menu
        //if scene = "start" then this.main_menu
        //else this.pause_menu
    
    (*
    member private __.size_menu =
        let option_list = ["Resume"; "Restart"; "Solution"; "Exit"]
        this.menu option_list
    *)

       