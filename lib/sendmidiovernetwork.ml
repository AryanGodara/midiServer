open Lwt.Infix

(* Create a UDP socket *)
let socket = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_DGRAM 0

(* Set the socket options *)
let () =
  Lwt_unix.setsockopt socket Lwt_unix.SO_REUSEADDR true;
  Lwt_unix.setsockopt socket Lwt_unix.SO_BROADCAST true

(* Bind the socket to a local address *)
let local_addr = Unix.ADDR_INET (Unix.inet_addr_any, 5004)
let () = Lwt_unix.bind socket local_addr

(* Define the remote address *)
let remote_addr = Unix.ADDR_INET (Unix.inet_addr_of_string "192.168.1.255", 5004)

(* Create a MIDI message *)
let message =
  RTPMIDI.MIDI_MESSAGE.{ status = 0x90; channel = 0; data1 = 60; data2 = 100 }

(* Convert the MIDI message to bytes *)
let bytes = RTPMIDI.midi_to_bytes message

(* Send the MIDI message as an RTP packet *)
let send_packet () =
  let packet = Rtp.Packet.create ~payload:bytes () in
  let packet_bytes = Rtp.Packet.to_cstruct packet in
  Lwt_unix.sendto socket
    (Cstruct.to_bytes packet_bytes)
    0 (Bytes.length bytes) [] remote_addr

(* Send the MIDI message repeatedly every 100ms *)
let rec send_messages () =
  Lwt_unix.sleep 0.1 >>= fun () ->
  send_packet () >>= fun _ -> send_messages ()

(* Start sending MIDI messages *)
let () = Lwt_main.run (send_messages ())
