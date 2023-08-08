module type RTP_MIDI = sig

  module MIDI_MESSAGE : sig
    type t
    (* type t = STATUS_BYTE of t | DATA_BYTE of t *)
    (* = {
      status : int;
      channel : int;
      data1 : int;
      data2 : int;
      timestamp : int;
    } *)
    
    val create :
      status:int -> channel:int -> data1:int -> data2:int -> timestamp:int -> t

    (* Getters *)
    val status : t -> int
    val channel : t -> int
    val data1 : t -> int
    val data2 : t -> int
    val timestamp : t -> int
  end


  module SERIALIZER : sig
    type t = MIDI_MESSAGE.t

    val serialize : t -> bytes
    val deserialize : bytes -> t
  end

  val midi_to_bytes : MIDI_MESSAGE.t -> Bytes.t
  (** Convert a MIDI message to a byte string *)

  val bytes_to_midi : Bytes.t -> MIDI_MESSAGE.t
  (** Convert a byte string to a MIDI message *)
end

module RTPMIDI : RTP_MIDI = struct
  module MIDI_MESSAGE = struct
    type t = {
      status : int;
      channel : int; 
      data1 : int;
      data2 : int;
      timestamp : int;
    }

    let create ~status ~channel ~data1 ~data2 ~timestamp =
      { status; channel; data1; data2; timestamp}

    let status message = message.status
    let channel message = message.channel
    let data1 message = message.data1
    let data2 message = message.data2
    let timestamp message = message.timestamp
  end

  module SERIALIZER = struct
    (* open MIDI_MESSAGE *)

    type t = MIDI_MESSAGE.t

    let serialize t =
      let bytes = Bytes.create 4 in
      Bytes.set bytes 0 (Char.chr t.MIDI_MESSAGE.status);
      Bytes.set bytes 1 (Char.chr t.MIDI_MESSAGE.channel);
      Bytes.set bytes 2 (Char.chr t.MIDI_MESSAGE.data1);
      Bytes.set bytes 3 (Char.chr t.MIDI_MESSAGE.data2);
      bytes

    let deserialize bytes =
      if Bytes.length bytes <> 4 then failwith "Invalid MIDI message bytes";
      let status = int_of_char (Bytes.get bytes 0) in
      let channel = int_of_char (Bytes.get bytes 1) in
      let data1 = int_of_char (Bytes.get bytes 2) in
      let data2 = int_of_char (Bytes.get bytes 3) in
      MIDI_MESSAGE.create ~status ~channel ~data1 ~data2 ~timestamp:0
  end

  let midi_to_bytes (message : MIDI_MESSAGE.t) : Bytes.t =
    SERIALIZE.serialize message

  let bytes_to_midi (bytes : Bytes.t) : MIDI_MESSAGE.t =
    SERIALIZE.deserialize bytes
end




(* module type RTP_MIDI = sig
  module MIDI_MESSAGE : sig
    type t = {
      status : int;
      channel : int;
      data1 : int;
      data2 : int;
      timestamp : int;
    }

    val create :
      status:int -> channel:int -> data1:int -> data2:int -> timestamp:int -> t

    (* Getters *)
    val status : t -> int
    val channel : t -> int
    val data1 : t -> int
    val data2 : t -> int
    val timestamp : t -> int
  end

  module SERIALIZE : sig
    type t = MIDI_MESSAGE.t

    val serialize : t -> bytes
    val deserialize : bytes -> t
  end

  val midi_to_bytes : MIDI_MESSAGE.t -> Bytes.t
  (** Convert a MIDI message to a byte string *)

  val bytes_to_midi : Bytes.t -> MIDI_MESSAGE.t
  (** Convert a byte string to a MIDI message *)
end *)

(* module RTPMIDI : RTP_MIDI = struct
  module MIDI_MESSAGE = struct
    type t = {
      status : int;
      channel : int;
      data1 : int;
      data2 : int;
      timestamp : int;
    }

    let create ~status ~channel ~data1 ~data2 ~timestamp =
      { status; channel; data1; data2; timestamp }

    let status message = message.status
    let channel message = message.channel
    let data1 message = message.data1
    let data2 message = message.data2
    let timestamp message = message.timestamp
  end

  module SERIALIZE = struct
    open MIDI_MESSAGE

    type t = MIDI_MESSAGE.t

    let serialize t =
      Printf.sprintf "MIDI Message: status=%d channel=%d data1=%d data2=%d"
        t.status t.channel t.data1 t.data2

    let deserialize str =
      let status =
        Scanf.sscanf str "MIDI Message: status=%d channel=%d data1=%d data2=%d"
          (fun status channel data1 data2 -> (status, channel, data1, data2))
      in
      MIDI_MESSAGE.create_new ~status:(fst status) ~channel:(snd status)
        ~data1:(fst (snd status))
        ~data2:(snd (snd status))
        ~timestamp:0
  end

  let midi_to_bytes (message : MIDI_MESSAGE.t) : Bytes.t =
    let bytes = Bytes.create 4 in
    Bytes.set bytes 0 (Char.chr message.status);
    Bytes.set bytes 1 (Char.chr message.channel);
    Bytes.set bytes 2 (Char.chr message.data1);
    Bytes.set bytes 3 (Char.chr message.data2);
    bytes

  let bytes_to_midi (bytes : Bytes.t) : MIDI_MESSAGE.t =
    if Bytes.length bytes <> 4 then failwith "Invalid MIDI message bytes";
    let status = int_of_char (Bytes.get bytes 0) in
    let channel = int_of_char (Bytes.get bytes 1) in
    let data1 = int_of_char (Bytes.get bytes 2) in
    let data2 = int_of_char (Bytes.get bytes 3) in
    { status; channel; data1; data2 }
end *)
