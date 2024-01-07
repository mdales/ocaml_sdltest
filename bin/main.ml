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
  Sdl.render_fill_rect r None >|= fun() ->
  Sdl.render_present r

let tick1 t r = 
  let progress = (t / 10) mod 312 in
  Sdl.set_render_draw_color r 128 128 255 255 >>= fun () ->
  let inner = Sdl.Rect.create ~x:100 ~y:182 ~w:progress ~h:20 in
  Sdl.render_fill_rect r (Some inner) >>= fun() ->

  Sdl.set_render_draw_color r 20 20 20 255 >>= fun () ->
  let outer = Sdl.Rect.create ~x:100 ~y:182 ~w:312 ~h:20 in
  Sdl.render_draw_rect r (Some outer) 

let tick2 t r = 
  let t2 = t - (312 * 10) in

  Sdl.set_render_draw_color r 128 128 255 255 >>= fun () ->
  let inner = Sdl.Rect.create ~x:100 ~y:182 ~w:312 ~h:20 in
  Sdl.render_fill_rect r (Some inner) >>= fun() ->

  let drip = Sdl.Rect.create ~x:411 ~y:202 ~w:1 ~h:182 in
  Sdl.render_fill_rect r (Some drip) >>= fun() ->

  let height = min (t2 / 4) 384 in
  let level = Sdl.Rect.create ~x:0 ~y:(384 - height) ~w:512 ~h:height in
  Sdl.render_fill_rect r (Some level) >>= fun() ->

  Sdl.set_render_draw_color r 20 20 20 255 >>= fun () ->
  let outer = Sdl.Rect.create ~x:100 ~y:182 ~w:312 ~h:20 in
  Sdl.render_draw_rect r (Some outer) 

let tick t r =
  Sdl.render_clear r >>= fun () ->

  Sdl.set_render_draw_color r 240 240 240 255 >>= fun () ->
  Sdl.render_fill_rect r None >>= fun() ->

  let innert = t mod ((312 * 10) + (384 * 4)) in

  (if innert < (312 * 10) then 
    tick1 innert r 
  else 
    tick2 innert r) >|= fun () ->
    
  Sdl.render_present r

let () = 
  match init 512 384 "SDL2 Progress" with
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
