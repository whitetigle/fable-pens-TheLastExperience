module pen

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser
open Fable.Import.JS


let canvas = Browser.document.getElementsByTagName_canvas().[0]

let onResize _ =
  canvas.width <- window.outerWidth
  canvas.height <- window.outerHeight

Browser.window.addEventListener_resize( unbox onResize, false)
let ctx = canvas.getContext_2d()
onResize()
let ground = if canvas.height > 500. then 0.85 else 1.0


type Point = {
    x:float
    y:float
    w:float
    px:float
    py:float
    vx:float
    vy:float
}

type StructPoint = {
  p:Point
  f: (Point -> float -> float -> Point) option
}

type Link = {
    p0: StructPoint
    p1: StructPoint
    distance:float
    size:float
    light: float
    force:float
    image : HTMLCanvasElement
    shadow : HTMLCanvasElement
}

type StructLink = {
  p0 : int
  p1 : int
  size : float
  lum : float
  disk: int option
}
type Struct = {
  points: StructPoint list
  links: StructLink list
}

module Robot =

  type Skeletton = {
      x : float
      points : StructPoint list
      links : Link list
      frame : int
      dir : int
      size : float
      color : float
      light : float
  }

  let standard =
    {
      x = 0.
      points = []
      links = []
      frame = 0
      dir = 1
      size = 4.
      color = 0.
      light = 80.
    }

  let defaultStructure =
    let makeStructPoint x y f =
      {
        p={
          x = x
          y = y
          w = 0.5
          px = x
          py = y
          vx = 0.0
          vy = 0.0
        }
        f=f
      }

    let empty = makeStructPoint 0. 0. None

    {
      points =
        [
          { empty
            with
              p={ empty.p with x=0.; y=4. }
              f=Some (fun p s d -> { p with y = p.y - 0.01 * s })
          }
          { empty
            with
              p={ empty.p with x=0.; y= -16. }
              f=Some (fun p s d -> { p with y = p.y - 0.02 * s })
          }
          { empty
            with
              p={ empty.p with x=0.; y=12. }
              f=Some (fun p s d -> { p with y = p.y + 0.02 * s })
          }
          { empty
            with
              p={ empty.p with x= -12.; y=0. }
          }
          { empty
            with
              p={ empty.p with x= 12.; y=0. }
          }
          { empty
            with
              p={ empty.p with x= -3.; y=34. }
              f = Some ( fun p s d ->
                if d > 0. then
                  { p with x = p.x + 0.01 * s; y = p.y - 0.015 * s }
                else
                  { p with y = p.y + 0.02 * s * d }
              )
          }
          { empty
            with
              p={ empty.p with x= 3.; y=34. }
              f = Some ( fun p s d ->
                if d > 0. then
                  { p with y = p.y + 0.002 * s }
                else
                  { p with x = p.x - 0.01 * s;  y = p.y - 0.015 * s }
            )
          }
          { empty
            with
              p={ empty.p with x= -28.; y=0. }
              f = Some ( fun p s d ->
                { p with x = p.x + p.vx * 0.035;  y = p.y - 0.001 * s }
            )
          }
          { empty
            with
              p={ empty.p with x= 28.; y=0. }
              f = Some ( fun p s d ->
                { p with x = p.x + p.vx * 0.035;  y = p.y - 0.001 * s }
            )
          }
          { empty
            with
              p={ empty.p with x= -3.; y=64. }
              f = Some ( fun p s d ->
                let y = p.y + 0.015 * s
                let y =
                  if d > 0. then
                    y - 0.01 * s
                  else
                    y + 0.05 * s
                { p with y = y }
            )
          }
          { empty
            with
              p={ empty.p with x= 3.; y=64. }
              f = Some ( fun p s d ->
                let y = p.y + 0.015 * s
                let y =
                  if d > 0. then
                    y + 0.05 * s
                  else
                    y - 0.01 * s
                { p with y = y }
            )
          }
        ]
      links =
        [
          { p0= 3; p1= 7; size= 12.; lum= 0.5; disk= None }
          { p0= 1; p1= 3; size= 24.; lum= 0.5; disk= None }
          { p0= 1; p1= 0; size= 60.; lum= 0.5; disk= Some 1 }
          { p0= 5; p1= 9; size= 16.; lum= 0.5; disk= None }
          { p0= 2; p1= 5; size= 32.; lum= 0.5; disk= None }
          { p0= 1; p1= 2; size= 50.; lum= 1.; disk= None }
          { p0= 6; p1= 10; size= 16.; lum= 1.5; disk= None }
          { p0= 2; p1= 6; size= 32.; lum= 1.5; disk= None }
          { p0= 4; p1= 8; size= 12.; lum= 1.5; disk= None }
          { p0= 1; p1= 4; size= 24.; lum= 1.5; disk= None }
        ]
    }

  let dancers =
    [
      for i in 0..5 do
        let fi = float i
        yield
          { standard
            with
              color = JS.Math.round (fi * 360. / 7.)
              light = 80.
              size = 4.
              x = (fi + 2.) * canvas.width / 9.
          }
    ]

  let update dancer =
    let frame = dancer.frame + 1
    let dir = if frame % 20 = 0 then -dancer.dir else dancer.dir
    // ---- update links ----
    let links =
      dancer.links
        |> List.map( fun link ->
          let p0 = link.p0
          let p1 = link.p1
          let dx = p0.p.x - p1.p.x
          let dy = p0.p.y - p1.p.y
          let dist = Math.sqrt(dx * dx + dy * dy)
          if dist >= 0. then
            let tw = p0.p.w + p1.p.w
            let r1 = p1.p.w / tw
            let r0 = p0.p.w / tw
            let dz = (link.distance - dist) * link.force
            let sx = dx / dist * dz
            let sy = dy / dist * dz
            let p1 = { p1.p with x = p1.p.x - sx * r0; y = p1.p.y - sy * r0}
            let p0 = { p0.p with x = p0.p.x + sx * r1; y = p0.p.y + sy * r1}
            {link with p0={ link.p0 with p=p0};p1={ link.p1 with p=p1}}
          else
            link
        )
    printfn "*----------------------------"
    let points =
      dancer.points
        |> List.map( fun newPoint ->
          // ---- dance ----
          (*
          let newPoint =
            if p.f.IsSome then
              let fn = p.f.Value
              { p with p = fn p.p (16. * JS.Math.sqrt(dancer.size)) (float dir)}
            else p
          *)
          let point = newPoint.p

          // ---- verlet integration ----
          let vx = (point.x - point.px) * 0.995
          let vy = (point.y - point.py) * 0.995
          let updated =
            {
              point with
                vx = vx
                vy = vy
                px = point.x
                py = point.y
                x = point.x + vx
                y = point.y + vy + 0.01
            }
          // updated point
          { newPoint with p = updated }
        )
    (*
    // ---- ground ----
    let links =
      links
        |> List.map( fun link ->
          let p1 = link.p1
          if p1.p.y > canvas.height * ground - link.size * 0.5 then
            {
              link with
                p1 =
                {
                  p1 with
                    p =
                      {
                        p1.p with
                          y= canvas.height * ground - link.size * 0.5
                          x= p1.p.x - p1.p.vx
                          vx=0.
                          vy=0.
                      }
                }
            }
          else
            link
        )
    // ---- center position ----
    let delta = (dancer.x - points.Head.p.x ) * 0.0002
    let points =
      points
        |> List.mapi( fun i point ->
          if i = 9 || i = 10 then
            {point with p = { point.p with x= point.p.x + delta } }
          else
            point
        )
        *)
    { dancer with links=links;points=points;frame=frame;dir=dir}

  let draw dancer =
    dancer.links
      |> List.iter( fun link ->
          if link.size > 0. then
            let dx = link.p1.p.x - link.p0.p.x
            let dy = link.p1.p.y - link.p0.p.y
            let a = Math.atan2(dy, dx)
            let d = Math.sqrt(dx * dx + dy * dy)
            // ---- shadow ----
            ctx.save()
            ctx.translate(link.p0.p.x + link.size * 0.25, link.p0.p.y + link.size * 0.25)
            ctx.rotate(a)
            ctx.drawImage(
              !^link.shadow,
              -link.size * 0.5,
              -link.size * 0.5,
              d + link.size,
              link.size
            )
            ctx.restore()
            // ---- stroke ----
            ctx.save()
            ctx.translate(link.p0.p.x, link.p0.p.y)
            ctx.rotate(a)
            ctx.drawImage(
              !^link.image,
              -link.size * 0.5,
              -link.size * 0.5,
              d + link.size,
              link.size
            )
            ctx.restore()
      )
    dancer

module Dancers =

  let start (dancers:Robot.Skeletton list) y =

    let stroke color axis (disk:int option) dist size =
      let image = document.createElement("canvas") :?> HTMLCanvasElement
      image.width <- dist + size
      image.height <- size
      let ict = image.getContext_2d()
      ict.beginPath()
      ict.lineCap <- "round"
      ict.lineWidth <- size
      ict.strokeStyle <- color

      if disk.IsSome then
        ict.arc(size * 0.5 + dist, size * 0.5, size * 0.5, 0., 2. * JS.Math.PI)
        ict.fillStyle <- color
        ict.fill()
      else
        ict.moveTo(size * 0.5, size * 0.5)
        ict.lineTo(size * 0.5 + dist, size * 0.5)
        ict.stroke()

      if axis then
        let s = size / 10.
        ict.fillStyle <- !^"#000"
        ict.fillRect(size * 0.5 - s, size * 0.5 - s, s * 2., s * 2.)
        ict.fillRect(size * 0.5 - s + dist, size * 0.5 - s, s * 2., s * 2.)

      image

    dancers
      |> List.map( fun dancer ->
        let points =
          Robot.defaultStructure.points
            |> List.map( fun p ->
              let x = dancer.size * p.p.x + dancer.x
              let y = dancer.size * p.p.y + y
              let pt =
                {
                  p.p with
                    x= x
                    y= y
                    px = x
                    py = y
                }
              { p with p = pt }
            )
        let links =
          Robot.defaultStructure.links
            |> List.map( fun link ->
              let p0 = points.[link.p0]
              let p1 = points.[link.p1]
              let dx = p0.p.x - p1.p.x
              let dy = p0.p.y - p1.p.y
              let distance = JS.Math.sqrt(dx * dx + dy * dy)
              let size = float( link.size) * dancer.size / 3.
              let imageColor = sprintf "hsl(%i,30%%,%i%%)" (int dancer.color) ((dancer.light * link.lum) |> int)
              {
                p0 = p0
                p1 = p1
                distance = distance
                size = size
                light = link.lum
                force = 0.5
                image = stroke !^imageColor true link.disk distance size
                shadow = stroke !^"rgba(0,0,0,0.5)" false link.disk  distance size
              }
            )
        {dancer with points=points; links=links}
      )

let run =
  let mutable tick = 0.
  let y = canvas.height * ground - 340.

  let initialDancers =
    Dancers.start Robot.dancers y
      |> List.mapi( fun i dancer ->
        let x = ( float i + 2. ) * canvas.width / 9.
        { dancer with x=x }
      )

  let mutable dancers = initialDancers
  let rec run (dt:float) =
    window.requestAnimationFrame(FrameRequestCallback run) |> ignore
    ctx.clearRect(0., 0., canvas.width, canvas.height)
    ctx.fillStyle <- !^"#222"
    ctx.fillRect(0., 0., canvas.width, canvas.height * 0.15)
    ctx.fillRect(0., canvas.height * 0.85, canvas.width, canvas.height * 0.15)
    dancers <- dancers |> List.map (Robot.update >> Robot.draw)
    tick <- tick + 0.1
  run

run 0.
