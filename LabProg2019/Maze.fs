(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Maze.fs: maze
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.Maze

open External
open Gfx

type CharInfo  with
    
    /// Shortcut for creating a wall pixel.
    static member wall (?fg : Color) = 
        let bg = Color.Black
        // shallpass = false = Black, true = White
        let shallpass = Color.Black
        let fg = defaultArg fg Color.Magenta
        pixel.create (Config.wall_pixel_char, fg, bg, shallpass)


    /// Shortcut for creating a visited cell given the foreground and background colors.
    static member visited (fg : Color, ?bg : Color) = CharInfo.create (Config.visited_pixel_char, fg, ?bg = bg)

    /// Shortcut for creating a deadend cell.
    static member deadend = 
        let fg = Color.Black
        let bg = Color.Black
        pixel.create (Config.deadend_pixel_char, fg, bg)
    /// Shortcut for creating a path pixel.
    static member internal path = pixel.filled Color.Black

    /// Check whether this pixel is a wall.
    member this.isWall = this.sp = Color.Black
    /// Check whether this pixel is visited.
    member this.isVisited= this.Char.UnicodeChar = Config.visited_pixel_char
    /// Check whether this pixel is a path solution pixel
    member this.isPath (fg : Color) = this.Char.UnicodeChar = Config.visited_pixel_char && this.fg = fg
    /// Check whether this pixel is deadend.
    member this.isDeadend= this = pixel.deadend


// TODO: implement the maze type, its generation (task 1) and its automatic resolution (task 2)
type maze (w : int, h : int, fpx : pixel, px : pixel) as this =
    inherit image (w, h, fpx) 
    // TODO: do not forget to call the generation function in your object initializer
    do this.generate

    /// maze generation: recursive subdivision algorythm
    member private __.subdivision = 
        let x0 = 0
        let y0 = 0
        let x1 = w
        let y1 = h

        this.draw_rectangle (x0, y0, x1, y1, px) // rectangle counts the 1-gap in width

        // rightful coordinates
        let rx1 = x0 + x1-1
        let ry1 = y0 + y1-1

        // start and exit
        this.plot(x0, y0+1, pixel.empty)
        this.plot(rx1-1, ry1, pixel.empty)
        
        let rec divide ((x0, y0),(x1, y1)) = 
            // condition to draw lines
            let predx = x1 - x0 > 2
            let predy = y1 - y0 > 2
            
            if predx && predy then
                let rnd_x = 
                    if ((x0/2)+1) < ((x1/2)-1) then (rnd_int ((x0/2)+1) ((x1/2)-1)) * 2
                    else x0 + 2
                let rnd_y = 
                    if ((y0/2)+1) < ((y1/2)-1) then (rnd_int ((y0/2)+1) ((y1/2)-1)) * 2
                    else y0 + 2
    
                this.draw_line (rnd_x, y0, rnd_x, y1, px)
                this.draw_line (x0, rnd_y, x1, rnd_y, px)

                // holes
                let hole_inf_y = (rnd_int (y0/2) ((rnd_y-2)/2))*2 + 1
                let hole_sup_y = (rnd_int (rnd_y/2) ((y1-2)/2))*2 + 1 
                let hole_inf_x = (rnd_int (x0/2) ((rnd_x-2)/2))*2 + 1 
                let hole_sup_x = (rnd_int (rnd_x/2) ((x1-2)/2))*2 + 1 

                let rnd_close = rnd_int 0 3 
        
                if rnd_close <> 0 then this.plot(rnd_x, hole_inf_y, pixel.empty)
                if rnd_close <> 1 then this.plot(rnd_x, hole_sup_y, pixel.empty)
                if rnd_close <> 2 then this.plot(hole_inf_x, rnd_y, pixel.empty)
                if rnd_close <> 3 then this.plot(hole_sup_x, rnd_y, pixel.empty)

                let quadrant_1 = ((rnd_x, y0), (x1, rnd_y))
                let quadrant_2 = ((x0, y0), (rnd_x, rnd_y))
                let quadrant_3 = ((x0, rnd_y), (rnd_x, y1))
                let quadrant_4 = ((rnd_x, rnd_y), (x1, y1))
                  
                divide quadrant_1
                divide quadrant_2
                divide quadrant_3
                divide quadrant_4      

        in divide ((x0, y0), (rx1, ry1))
      
    member private __.subdiv_backtracking = 
        
        let x1 = w
        let y1 = h
        let rx1 = x1-1
        let ry1 = y1-1
        let rec backtracking x0 y0 = 
           if x0 < rx1 && y0 < ry1 then
               this.plot(x0, y0, pixel.visited Color.Black)
               //aliases for neighbours
               let nb_u = this.[x0, y0-1]
               let nb_r = this.[x0+1, y0]
               let nb_d = this.[x0, y0+1]
               let nb_l = this.[x0-1, y0] 

               //start
               if x0 = 0 && y0 = 1 then  backtracking (x0+1) y0
               else
                    
                   // tracking the path in all directions
                   if (nb_u.isWall || nb_u.isVisited) = false  then backtracking x0 (y0-1)
                   if (nb_r.isWall || nb_r.isVisited) = false  then backtracking (x0+1) y0
                   if (nb_d.isWall || nb_d.isVisited) = false  then backtracking x0 (y0+1)
                   if (nb_l.isWall || nb_l.isVisited) = false  then backtracking (x0-1) y0
                   

                   let rec go_back (x0, y0) = 
                       // BACKTRACKING
                       //aliases for neighbours
                       let nb_u = this.[x0, y0-1]
                       let nb_r = this.[x0+1, y0]
                       let nb_d = this.[x0, y0+1]
                       let nb_l = this.[x0-1, y0]
                       //aliases for coord neighbours couples
                       let c_u = (x0, (y0-1))
                       let c_r = ((x0+1), y0)
                       let c_d = (x0, (y0+1))
                       let c_l = ((x0-1), y0)
                       let c_0 = (x0, y0)
                        // check if dead end
                       let nb_list = [(nb_u.isWall, c_u); (nb_r.isWall, c_r); (nb_d.isWall, c_d); (nb_l.isWall, c_l)]
                       //let nb_list = [nb_u.isWall; nb_r.isWall; nb_d.isWall; nb_l.isWall]
                       let de_list = [(nb_u.isDeadend, c_u); (nb_r.isDeadend, c_r); (nb_d.isDeadend, c_d); (nb_l.isDeadend, c_l)]
                       //check neighbours if wall or visited and return a 4 digits integer
                       let check_neighbours l1 l2 = 
                            let rec aux l1 l2 sum (acc,coord)= 
                                match (l1, l2) with
                                | ([], []) | ([], _) | (_,[]) -> (acc, coord)                        
                                | ((z, c_1):: zs, (r, c_2)::rs) -> 
                                            if z || r
                                            then aux zs rs (sum*10) ((acc + sum), coord)
                                            else aux zs rs (sum*10) (acc, c_1)
                            in aux l1 l2 1 (0, c_0) 
                        
                       // sum each digit 
                       let check_n n =
                            let rec aux n1 acc =
                                if n1 <= 0 then acc
                                else aux (n1/10) (acc + (n1%10))
                            in aux n 0  
                       
                       let (nb_int, nb_coord) = check_neighbours nb_list de_list   
                       if check_n nb_int  = 3 
                       then  
                            this.plot(x0, y0, pixel.deadend)
                            go_back nb_coord
                   go_back (x0, y0)             
                    
           else if y0 = ry1 then 
                this.plot(x0, y0, pixel.visited Color.Black)


           in backtracking 0 1

    member private __.deep_tree = 
        let xi = 0
        let yi = 0
        let x1 = w
        let y1 = h

        let rec fill_maze (xi, yi, x1, y1) =           
            this.draw_rectangle (xi, yi, x1, y1, px) // rectangle counts the 1-gap in width
            if y1 = 1 then this.draw_line (xi, yi, x1, y1, px)
            else fill_maze (xi+1, yi+1, x1-2, y1-2)
        fill_maze (xi, yi, x1, y1)

        // rightful coordinates
        let rx1 = xi + x1-1
        let ry1 = yi + y1-1

        // start and exit
        this.plot(xi, yi+1, pixel.empty)
        this.plot(rx1-1, ry1, pixel.empty)
        
        let rnd_w = (rnd_int 2 (w-1)/2) * 2 - 1
        let rnd_h = (rnd_int 2 (h-1)/2) * 2 - 1
        //this.plot (1, 1, pixel.empty)
        let graph = [(rnd_w, rnd_h)]
        
        let rec deeptree x0 y0 l_graph = 
            // check if the point is in the maze
            let is_in_maze (x0, y0) =
                let coord_out_maze coord border = coord < 0 || coord > (border - 1)
                not (coord_out_maze x0 w || coord_out_maze y0 h)
            // list of in maze neighbours
            let get_neighbours (x0, y0) = 
                //aliases for coord neighbours couples
                let c_u = (x0, (y0-2))
                let c_r = ((x0+2), y0)
                let c_d = (x0, (y0+2))
                let c_l = ((x0-2), y0)
                let nb_coord_list = [c_u; c_r; c_d; c_l]
                List.filter (fun nb -> is_in_maze nb) nb_coord_list
            
            // list of not visited neighbours
            let not_visited_list = 
                 let rec aux l = 
                     match l with
                     | [] -> []
                     | (nx, ny) :: xs -> 
                         if this.[nx, ny].isWall = true then  (nx, ny) :: aux xs
                         else aux xs
                 aux (get_neighbours (x0, y0))            
            // get list length
            let list_length = List.length not_visited_list  
            
            // if length = 0 then stop and try again with last couple in graph else plot
            if list_length = 0 then 
                match l_graph with
                | [] -> this.plot (x0, y0, pixel.empty)
                | (lx, ly) :: gs -> deeptree lx ly gs
            else
                // rnd int to choose an item
                let nb = rnd_int 0 (list_length - 1)
                // item to plot
                let n_coord =
                    let rec aux l acc =
                        match l with
                        | [] -> (x0, y0)
                        | z :: ls -> 
                            if acc = 0 then z
                            else aux ls (acc-1)
                    in aux not_visited_list nb
                let xt, yt = n_coord
                this.draw_line (x0, y0, xt, yt, pixel.empty)
                deeptree xt yt ((xt, yt)::l_graph) 
               
        deeptree rnd_w rnd_h graph
        


    // TODO: start with implementing the generation
    /// maze generation: random choice between 2 algorythms
    member private __.generate = 
        let coin = rnd_int 0 10
        if coin <= 5 then this.subdivision
        else this.deep_tree
        this.subdiv_backtracking
        
/// Sprite extension
type sprite with
    /// show / hide path changing pixel colors
    /// Parameters: oc = old color, nc = new color, with, height
    member this.change_color (oc : Color, nc : Color, w: int, h: int) =
        let xi = 0
        let yi = 1

        //aliases for neighbours
        let c_r = ((xi+1), yi)
        
        this.plot(xi, yi, pixel.visited nc)
        
        let rec hide_path (x0, y0)  =
            this.plot(x0, y0, pixel.visited nc)
            //aliases for neighbours
            let nb_u = this.[x0, y0-1]
            let nb_r = this.[x0+1, y0]
            let nb_d = this.[x0, y0+1]
            let nb_l = this.[x0-1, y0] 

            //aliases for coord neighbours couples
            let c_u = (x0, (y0-1))
            let c_r = ((x0+1), y0)
            let c_d = (x0, (y0+1))
            let c_l = ((x0-1), y0)
            let c_0 = (x0, y0)
           
            // check every neighbour if it's a solution pixel or not
            let nb_list = [(nb_u.isPath (oc), c_u); (nb_r.isPath (oc), c_r); (nb_d.isPath (oc), c_d); (nb_l.isPath (oc), c_l)]
            // find the right neighbour
            let rec coord_path nb_list = 
                match nb_list with 
                | [] ->  c_0
                | (b, coord) :: xs -> if b then coord
                                      else coord_path xs
            let next_one = coord_path nb_list
            
            // if next_one = x0, y0 then end, else change color and go on
            if next_one = c_0 then this.plot(x0, y0, pixel.visited nc)
            else if next_one = ((w-2), (h-1)) then this.plot((w-2), (h-1), pixel.visited nc)
            else hide_path next_one
            
        hide_path c_r
        