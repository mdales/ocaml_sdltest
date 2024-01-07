open Tsdl

let (>>=) = Result.bind
let (>|=) v f = Result.map f v

let init () = 
  Sdl.init Sdl.Init.(video + events) >>= fun () -> 
  Sdl.create_window ~w:640 ~h:480 "SDL OpenGL" Sdl.Window.opengl >>= fun w -> 
  Sdl.create_renderer w >|= 
  fun r -> (w, r)

let () = 
  match init () with
  | Error (`Msg e) -> Sdl.log "Init error: %s" e; exit 1
  | Ok (w, r) ->  
    match Sdl.render_clear r with 
    | Error (`Msg e) -> Sdl.log "Clear renderer error: %s" e; exit 1
    | Ok () -> 
      match Sdl.set_render_draw_color r 255 0 0 255 with
      | Error (`Msg e) -> Sdl.log "Set color error: %s" e; exit 1
      | Ok () -> 
        match Sdl.render_draw_line r 10 10 100 100 with 
        | Error (`Msg e) -> Sdl.log "draw line error: %s" e; exit 1
        | Ok () -> 
          Sdl.render_present r;

          let rec loop () = (
            let e = Sdl.Event.create () in
            match Sdl.poll_event (Some e) with
            | true -> (
              match Sdl.Event.(enum (get e typ)) with
              | `Quit -> ()
              | _ -> loop ()
            )
            | false -> loop()
          ) in loop ();
          Sdl.destroy_renderer r;
          Sdl.destroy_window w;
          Sdl.quit ();
          exit 0
