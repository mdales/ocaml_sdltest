open Tsdl

let init () = 
  let a = Sdl.init Sdl.Init.(video + events) in
  let rw = Result.bind a (fun () -> Sdl.create_window ~w:640 ~h:480 "SDL OpenGL" Sdl.Window.opengl) in
  match Result.bind rw Sdl.create_renderer with
  | Error e -> Error e
  | Ok r -> Ok (Result.get_ok rw, r)

let main() = 
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


let _main () = 
  match Sdl.init Sdl.Init.(video + events) with
  | Error (`Msg e) -> Sdl.log "Init error: %s" e; exit 1
  | Ok () ->
    match Sdl.create_window ~w:640 ~h:480 "SDL OpenGL" Sdl.Window.opengl with
    | Error (`Msg e) -> Sdl.log "Create window error: %s" e; exit 1
    | Ok w ->
      match Sdl.create_renderer w with 
      | Error (`Msg e) -> Sdl.log "Create renderer error: %s" e; exit 1
      | Ok r ->
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

let () = main ()