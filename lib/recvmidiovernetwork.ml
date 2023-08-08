open Lwt.Infix

(* Define the local address to bind to *)
let local_addr = Unix.ADDR_INET (Unix.inet_addr_any, 5004)

(* Create a UDP socket *)
let socket = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_DGRAM 0

(* Set the socket options *)
let () =
  Lwt_unix.setsockopt socket Lwt_unix.SO_REUSEADDR true;
  Lwt_unix.setsockopt socket Lwt_unix.SO_BROADCAST true

(* Bind the socket to the local address *)
let () = Lwt_unix.bind socket local_addr

(* Initialize the RTP-MIDI session *)
let () = RTPMIDI.init ()

(* Receive and process RTP-MIDI packets *)
let rec receive_packets () =
  match RTPMIDI.receive_rtpmidi () with
  | Some packet ->
    let midi_message = RTPMIDI.rtpmidi_to_midi packet.midi_data in
    MidiDataStorage.store_midi midi_message;
    receive_packets ()
  | None ->
    Lwt_unix.sleep 0.1 >>= fun () ->
    receive_packets ()

(* Start receiving RTP-MIDI packets *)
let () = Lwt_main.run (receive_packets ())