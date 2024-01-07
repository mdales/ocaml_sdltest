open Tsdl

let (>>=) = Result.bind
let (>|=) v f = Result.map f v

let init (width : int) (height : int) (title : string) = 
  Sdl.init Sdl.Init.(video + events) >>= fun () -> 
  Sdl.create_window ~w:width ~h:height title Sdl.Window.opengl >>= fun w -> 
  Sdl.create_renderer w >|= 
  fun r -> (w, r)

let boot r = 
  Sdl.render_clear r >>= fun () ->
  Sdl.set_render_draw_color r 240 240 240 255 >>= fun () ->
  let rect = Sdl.Rect.create ~x:0 ~y:0 ~w:512 ~h:384 in
  Sdl.render_fill_rect r (Some rect) >|= fun() ->
  Sdl.render_present r

let tick t r =
  Sdl.render_clear r >>= fun () ->

  Sdl.set_render_draw_color r 240 240 240 255 >>= fun () ->
  let fullrect = Sdl.Rect.create ~x:0 ~y:0 ~w:512 ~h:384 in
  Sdl.render_fill_rect r (Some fullrect) >>= fun() ->

  Sdl.set_render_draw_color r 20 20 20 255 >>= fun () ->
  Sdl.render_draw_line r (t mod 512) 10 100 100 >|= fun() ->
    
  Sdl.render_present r

let () = 
  match init 512 384 "SDL Test" with
  | Error (`Msg e) -> Sdl.log "Init error: %s" e; exit 1
  | Ok (w, r) ->
    match boot r with 
    | Error (`Msg e) -> Sdl.log "Boot error: %s" e; exit 1
    | Ok () ->
      let t = ref 0 in
      let rec loop () = (
        match tick !t r with 
        | Error (`Msg e) -> Sdl.log "Boot error: %s" e
        | Ok () ->
          t := !t + 1;
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
