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

(* Receive and process RTP-MIDI packets *)
let rec receive_packets () =
  let buffer = Bytes.create 1024 in
  Lwt_unix.recvfrom socket buffer 0 (Bytes.length buffer) [] >>= fun (len, remote_addr) ->
  let packet = Rtp.Packet.of_cstruct (Cstruct.of_bytes buffer) in
  let midi_data = RTPMIDI.SERIALIZE.deserialize (Cstruct.to_string packet.payload) in
  let midi_message = RTPMIDI.bytes_to_midi (Bytes.of_string midi_data) in
  MidiDataStorage.store_midi midi_message;
  receive_packets ()

(* Start receiving RTP-MIDI packets *)
let () = Lwt_main.run (receive_packets ())