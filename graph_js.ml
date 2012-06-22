module Html = Dom_html
open String
let (>>=) = Lwt.bind
let js = Js.string
let document = Html.window##document

 let n=ref 0  (*number of blocks *)
let h=300.0 (* height of the graph*) 
let colour = Js.string "#000000"
let _ = Random.self_init ()

let button name callback =
  let res = document##createDocumentFragment() in
  let input = Html.createInput ~_type:(js"submit") document in
  input##value <- js name;
  input##onclick <- Html.handler callback;
  Dom.appendChild res input;
  res

let str_input name value =
  let res = document##createDocumentFragment() in
  Dom.appendChild res (document##createTextNode(js name));
  let input = Html.createInput ~_type:(js"text") document in
  input##value <- js ( !value);
  input##onchange <- Html.handler
    (fun _ ->
       begin try
         value :=  (Js.to_string (input##value))
       with Invalid_argument _ ->
         ()
       end;
       input##value <- js ( !value);
       Js._false);
  Dom.appendChild res input;
  res

let longstr_to_str k=let res=ref [] in
		       let st=ref 0 and en= ref 0 in
			   for i=0 to (String.length k)-1 do
				if (k.[i]=' ' && st<>en) then let tmp= String.sub k (!st) ((!en)-(!st)) in res:= tmp :: !res; 
														st:=i+1;en:=i+1
				else en:=!en + 1; if k.[i]=' ' then st:= (!en)
				done;
			let tmp= String.sub k (!st) (String.length k -(!st)) in res:= tmp :: !res;!res

let rec list_str_to_int =function
[]->[]
|x::xs -> (int_of_string x) :: list_str_to_int xs  

let create_canvas () =
  let d = Html.window##document in
  let c = Html.createCanvas d in
  c##width <- 300;
  c##height <- truncate h;
  c


let rec drawdata c data start= match data with
[]-> ()
|x::xs ->
  c##fillStyle <- colour;
  c##beginPath ();
  c##moveTo (10. +. start *. 30. ,h );
  c##lineTo (10. +. start *. 30. +. 20.,  h);
  c##lineTo (10. +. start *. 30. +. 20.,  h -. (float  x) *. 10.);
  c##lineTo (10. +. start *. 30.,  h -. (float x) *. 10.);
  c##fill ();
  drawdata c xs (start +. 1.)

let draw ctx canvas a =
  let c = canvas##getContext (Html._2d_) in
  drawdata c a 0.;
  ctx##drawImage_fromCanvas (canvas, 0., 0.)




let onload _ =
  let main =
    Js.Opt.get (Html.document##getElementById(js"main"))
      (fun () -> assert false)
  in
  let c = create_canvas () in
   let c' = create_canvas () in
   Dom.appendChild Html.window##document##body c;
     let c = c##getContext (Html._2d_) in
      c##globalCompositeOperation <- Js.string "copy";

  let num = ref "" in
  Dom.appendChild main (str_input "Numbers" num);
  Dom.appendChild main (Html.createBr document);
  Dom.appendChild main
    (button "Draw graph"
       (fun _ ->
       	  let data= list_str_to_int (longstr_to_str !num) in
            n:= List.length data; 
            draw c c' (List.rev data);        
          Js._false));
    
  Js._false

let _ = Html.window##onload <- Html.handler onload
