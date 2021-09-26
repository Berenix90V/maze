(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Game.fs: test modules
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)


module LabProg2019.Game

open System
open Engine
open Gfx
open Maze
open Menu

[< NoEquality; NoComparison >]
type state = {
    player : sprite
    mazy : sprite
    menu : sprite option
    pointer : sprite option
}

 (*
[< NoEquality; NoComparison >]
type main_state = {
    player : sprite
    mazy : sprite
    menu : sprite option
    pointer : sprite option
}

[< NoEquality; NoComparison >]
type menu_state = {
    menu : sprite
    //pointer : sprite
}

[< NoEquality; NoComparison >]
type state = State1 of main_state | State2 of menu_state
*)


/// set the menu pointer movement: up, down 
let pointer_movement (key: ConsoleKeyInfo, screen : wronly_raster, st: state) = 
    let dx, dy =
        match key.KeyChar with 
        | 'w' -> 0., -5.
        | 's' -> 0., 5.
        | _   -> 0., 0.
    // check menu border: pointer cannot go out
    let ddy = 
        match st.menu with
        | Some menu -> 
            let position_x = 
                match st.pointer with 
                | Some pointer -> int pointer.x
                | None -> 0
            let position_y = 
                match st.pointer with 
                | Some pointer -> int pointer.y + int dy
                | None -> 0                   
            let color =  menu.[position_x, position_y].op
            if color = Color.Black then 0.
            else dy

        | None -> 0.
    (dx, ddy)
/// set player movement: up, right, down, left
let player_movement (key: ConsoleKeyInfo, screen : wronly_raster, st: state) = 
    let dx, dy =
        match key.KeyChar with 
        | 'w' -> 0., -1.
        | 's' -> 0., 1.
        | 'a' -> -1., 0.
        | 'd' -> 1., 0.
        | _   -> 0., 0.

    //check bounds       
    let dx = if int st.player.x + st.player.width + int dx > screen.width || int st.player.x + int dx < 0 then 0. else dx
    let dy = if int st.player.y + st.player.height + int dy > screen.height || int st.player.y + int dy < 0 then 0. else dy
        
    //check walls       
    let coord_x = int st.player.x + int dx
    let position_y = int st.player.y 
 
    let coord_y = int st.player.y + int dy
    let position_x = int st.player.x 
    
    let dx = if st.mazy.[coord_x, position_y].isWall then 0. else dx
    let dy = if st.mazy.[position_x, coord_y].isWall then 0. else dy  
    (dx, dy)

/// get the option where the menu pointer is, it's stored as a Color type
let get_color (key: ConsoleKeyInfo, screen : wronly_raster, st: state) =     
     
    match st.menu with
    | Some menu -> 
        let position_x = 
            match st.pointer with 
            | Some pointer -> int pointer.x
            | None -> 0
        let position_y = 
            match st.pointer with 
            | Some pointer -> int pointer.y
            | None -> 0                   
        menu.[position_x, position_y].op

    | None -> Color.Black
/// Check where is the pointer in size menu and get the selected maze size. To change maze size insert only odd values of width and height
let get_size (key: ConsoleKeyInfo, screen : wronly_raster, st: state) = 
    match st.menu with
    | Some menu ->        
        let position_y = 
            match st.pointer with 
            | Some pointer -> int pointer.y
            | None -> 0                   
        match position_y with
        | 5 -> 43, 37
        | 10 -> 75, 41
        | 15 -> 101, 45
        | 20 -> 135, 49
        |_ ->  101, 45

    | None -> 83, 21

// insert only odd values
(*let W = 89
let H = 47*)

/// main game + pause and win menu
let main (w, h) =    
    let W = w
    let H = h

    let engine = new engine (W, H)

    /// Function to activate menu. 
    /// Parameters: key, raster, state, act : integer to detect the menu state (if it's present or not)
    /// act = 1 show on-click pause menu, act = 0 hide on-click pause menu, act = 3 show win menu
    let active_menu (key: ConsoleKeyInfo, screen : wronly_raster, st: state, act: int) =
   
        if act = 1 then
            let menu1 = engine.create_and_register_sprite (menu (W, H, pixel.filled Color.Green, "pause", W/10, pixel.filled Color.Yellow), 0, 0, 5) // z! w
            Some menu1
        else if act = 0 then
            match st.menu with 
            | Some menu -> engine.deregister_sprite menu                           
            | None -> st.player.z <- 3
            None
        else if act = 3 then
            let menu1 = engine.create_and_register_sprite (menu (W, H, pixel.filled Color.Green, "win", W/10, pixel.filled Color.Yellow), 0, 0, 5) // z! w
            Some menu1
        else st.menu 

    let my_update (key : ConsoleKeyInfo) (screen : wronly_raster) (st : state) =
        
        // USEFUL MUTABLES
        let mutable quit_condition = (key.KeyChar = 'q')
        let mutable player = st.player
        let mutable mazy = st.mazy

        // USEFUL FUNCTIONS
        // show/hide solution
        let solution () = 
            let path = 
                if st.mazy.[1, 1].fg = Color.DarkCyan then 0
                else 1
            if path = 0 then st.mazy.change_color(Color.DarkCyan, Color.Black, W, H)
            if path = 1 then st.mazy.change_color(Color.Black, Color.DarkCyan, W, H)
        // New maze generation
        let new_maze () =            
            engine.deregister_sprite st.mazy
            engine.deregister_sprite st.player
            mazy <- engine.create_and_register_sprite (maze (W, H, pixel.empty, pixel.wall Color.Magenta), 0, 0, 2)
            player <- engine.create_and_register_sprite (image.rectangle (1, 1, pixel.filled Color.Yellow, pixel.filled Color.Blue), 0, 1, 3)
        
        // ON-CLICK SHORTCUT
        // show solution
        if key.KeyChar = 'p' then solution ()
        // new maze
        if key.KeyChar = 'n' then new_maze ()
        // active menu on key
        let mutable act =
            match key.KeyChar with 
            | 'm' -> if st.menu = None then 1
                     else 0
            |_ -> 2
        

        // set menu sprite   
        let pos_x = int st.player.x
        let pos_y = int st.player.y
        if (pos_x, pos_y) = (w-2, h-2) && key.KeyChar = 's' then act <- 3
        let mutable menu = active_menu (key, screen, st, act)

        // set pointer sprite   
        let mutable pointer =
            if act = 1 || act = 3 then
                let pointer = engine.create_and_register_sprite (image.rectangle (1, 1, pixel.pointer Color.White), (W/10 + 2), 5, 6)
                Some pointer
            else if act = 0 then
                match st.pointer with 
                | Some pointer -> engine.deregister_sprite pointer                           
                | None -> st.player.z <- 3
                None
            else st.pointer
        

        // set player or pointer movement and menu options
        match st.pointer with 
        | Some pointer1 ->  
            let dx, dy = pointer_movement (key, screen, st)
            pointer1.move_by (dx, dy) 
            match st.menu with
            | Some menu1 -> 
                // get selected option
                let col = 
                    if key.KeyChar = '\013' then get_color (key, screen, st)
                                     else Color.Black
                let close_menu () =
                    engine.deregister_sprite pointer1
                    engine.deregister_sprite menu1
                    menu <- None
                    pointer <- None
                // menu options and related on click functions
                match col with
                | Color.Blue -> 
                    close_menu ()
                | Color.DarkBlue -> 
                    close_menu ()
                    st.player.x <- float 0
                    st.player.y <- float 1
                | Color.Cyan -> 
                    close_menu ()
                    solution()
                | Color.DarkGreen -> 
                    close_menu()
                    new_maze ()
                | Color.Red -> 
                    quit_condition <- true
                | _ -> st.player.z <- 3
                
            | None -> st.player.z <- 3
            
        | None -> let dx, dy = player_movement (key, screen, st)
                  st.player.move_by (dx, dy)      
                 
        let st = {
            player = player
            mazy = mazy
            menu = menu
            pointer = pointer
        }  

        st, quit_condition

    // create simple backgroud and player, menu and pointer set on None   
    let mazy = engine.create_and_register_sprite (maze (W, H, pixel.empty, pixel.wall Color.Magenta), 0, 0, 2)
    let player = engine.create_and_register_sprite (image.rectangle (1, 1, pixel.filled Color.Yellow, pixel.filled Color.Blue), 0, 1, 3)
    let menu = None
    let pointer = None
   

    // initialize state
    let st0 = { 
        player = player
        mazy = mazy
        menu = menu
        pointer = pointer
        }
   
    // start engine
    engine.loop_on_key my_update st0
   

(*  
let W1 = 21
let H1 = 23
*)
//let mutable w,h = 29,31

/// start menu
let start_menu () =
    let W1 = 50
    let H1 = 31


    let engine = new engine (W1, H1)
    let my_update (key : ConsoleKeyInfo) (screen : wronly_raster) (st : state) = 
        let mutable menu_value = st.menu
        let mutable pointer_value = st.pointer
        let mutable pred = false
        //let mw, mh = 49, 43
        
       
        match st.pointer with 
            | Some pointer ->  
                let dx, dy = pointer_movement (key, screen, st)
                pointer.move_by (dx, dy)
            | None -> st.player.z <- 2
        let col = if key.KeyChar = '\013' then get_color (key, screen, st)
                    else Color.Black
        // quit condition
        pred <- (key.KeyChar = '\013' && col = Color.Red)

       
        // menu options
        match col with
        | Color.Green -> 
            let pred1 = (key.KeyChar = '\013')
            pred <- pred1
            //result
            let mw,mh = get_size (key, screen, st)
            (st, pred), pred, col, mw, mh
           
        | _ -> 
            match st.menu with
            | Some menu2 -> 
                match st.pointer with
                | Some pointer ->
                    
                    // function to get new menu scene
                    let new_menu () selected = 
                        // deregister old menu and pointer
                        engine.deregister_sprite menu2
                        engine.deregister_sprite pointer
                        // initialize new ones
                        let menu1 = engine.create_and_register_sprite (menu (W1, H1, pixel.filled Color.Blue, selected, W1/10, pixel.filled Color.Yellow), 0, 0, 3) // w
                        menu_value <- Some menu1
                        let pointer1 = engine.create_and_register_sprite (image.rectangle (1, 1, pixel.pointer Color.White), (W1/10 + 2), 5, 4)    
                        pointer_value <- Some pointer1
                    
                    match col with
                    // choose size maze
                    | Color.Yellow -> new_menu () "size"    
                    // click help
                    | Color.DarkMagenta -> new_menu () "help"
                    // go back to start menu from help menu
                    | Color.DarkRed -> new_menu () "start"

                    //set nothing priority to the samevalue only to complete the match
                    | _ -> st.player.z <- 2
                | None -> st.player.z <- 2
            | None -> st.player.z <- 2
            
            let pred1 = (key.KeyChar = '\013' && col = Color.Red)
            pred <- pred1
            let st = {
                player = st.player
                mazy = st.mazy
                menu = menu_value
                pointer = pointer_value
            }
            // return result
            (st, pred), pred, col, 89, 47
    let mazy = engine.create_and_register_sprite (image.rectangle (W1, H1, pixel.filled Color.Black, pixel.filled Color.Black), 0, 0, 1)
    let player = engine.create_and_register_sprite (image.rectangle (1, 1, pixel.filled Color.Black, pixel.filled Color.Black), 0, 1, 2)
    
    let menu1 = engine.create_and_register_sprite (menu (W1, H1, pixel.filled Color.Blue, "start", W1/10, pixel.filled Color.Yellow), 0, 0, 3) 
    let menu = Some menu1

    let pointer1 = engine.create_and_register_sprite (image.rectangle (1, 1, pixel.pointer Color.White), (W1/10 + 2), 5, 4)    
    let pointer = Some pointer1

    // start engine
    let m_st = {
        player = player
        mazy = mazy
        menu = menu
        pointer = pointer
    }
    
    engine.loop_on_key1 my_update m_st
    

   