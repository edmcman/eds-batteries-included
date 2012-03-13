(*
 * BatInnerIO - Abstract input/output (inner module)
 * Copyright (C) 2003 Nicolas Cannasse
 *               2008 David Teller
 *               2008 Philippe Strauss
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)


(**
   Core of the BatIO module.

   This module contains the core definitions of {!BatIO}, so as to avoid circular
   dependencies between modules which only need simple functions of {!BatIO} and
   that module itself.

   Don't use this module, use {!BatIO}.

   @author Nicolas Cannasse
   @author David Teller
   @author Philippe Strauss
   @author Edgar Friendly
*)

type -'cap input
type (-'cap, 'a) output

type 'cap inputRead = ([> `Read] as 'cap) input
(** The type of [input] that has at least the [Read] capability. *)

type ('cap, 'a) outputWrite = ([> `Write] as 'cap, 'a) output
(** The type of [output] that has at least the [Write] capability. *)

exception No_more_input
(** This exception is raised when reading on an input with the [read] or
  [nread] functions while there is no available token to read. *)

exception Input_closed
(** This exception is raised when reading on a closed input. *)

exception Output_closed
(** This exception is raised when reading on a closed output. *)

val read : _ input -> char
(** Read a single char from an input or raise [No_more_input] if
  no input available. *)

val read_all : _ input -> string
(** read all the contents of the input until [No_more_input] is raised. *)

val pipe : unit -> [`Read] input * ([`Write], unit) output
(** Create a pipe between an input and an ouput. Data written from
  the output can be read from the input. *)

val nread : _ input -> int -> string
(** [nread i n] reads a string of size up to [n] from an input.
  The function will raise [No_more_input] if no input is available.
  It will raise [Invalid_argument] if [n] < 0. *)

val really_nread : _ input -> int -> string
(** [really_nread i n] reads a string of exactly [n] characters
    from the input. Raises [No_more_input] if at least [n] characters are
    not available. Raises [Invalid_argument] if [n] < 0. *)

val input : _ input -> string -> int -> int -> int
(** [input i s p l] reads up to [l] characters from the given input, storing
  them in string [s], starting at character number [p]. It returns the actual
  number of characters read or raise [No_more_input] if no character can be
  read. It will raise [Invalid_argument] if [p] and [l] do not designate a
  valid substring of [s]. *)

val really_input : _ input -> string -> int -> int -> int
(** [really_input i s p l] reads exactly [l] characters from the given input,
  storing them in the string [s], starting at position [p]. For consistency with
  {!BatIO.input} it returns [l]. Raises [No_more_input] if at [l] characters are
  not available. Raises [Invalid_argument] if [p] and [l] do not designate a
  valid substring of [s]. *)

val seek_in : [> `Seek] input -> int -> unit
(** [seek_in inp pos] sets the current reading position to [pos]
   for input [inp]. *)

val close_in : _ input -> unit
(** Close the input. It can no longer be read from. *)

(*val auto_close_in : 'a input -> 'a input
(** Create a new channel which will close automatically once there is nothing
    left to read.*)*)

val write : (_, 'a) output -> char -> unit
(** Write a single char to an output. *)

val nwrite : (_, 'a) output -> string -> unit
(** Write a string to an output. *)

val output : (_, 'a) output -> string -> int -> int -> int
(** [output o s p l] writes up to [l] characters from string [s], starting at
  offset [p]. It returns the number of characters written. It will raise
  [Invalid_argument] if [p] and [l] do not designate a valid substring of [s]. *)

val really_output : (_, 'a) output -> string -> int -> int -> int
(** [really_output o s p l] writes exactly [l] characters from string [s] onto
  the the output, starting with the character at offset [p]. For consistency with
  {!BatIO.output} it returns [l]. Raises [Invalid_argument] if [p] and [l] do not
  designate a valid substring of [s]. *)

val seek_out : ([> `Seek], _) output -> int -> unit
(** [seek_out out pos] sets the current reading position to [pos]
   for output [out]. *)

val flush : (_, 'a) output -> unit
(** Flush an output. *)

val flush_all : unit -> unit
(** Flush all outputs. *)

val close_out : (_, 'a) output -> 'a
(** Close the output and return its accumulator data.
    It can no longer be written. *)

val close_all : unit -> unit
(** Close all outputs.
    Ignore errors.*)

val input_string : string -> [`Read] input
(** Create an input that will read from a string. *)

val output_string : unit -> ([`Write], string) output
(** Create an output that will write into a string in an efficient way.
  When closed, the output returns all the data written into it. *)

val on_close_out : ('cap, 'a) output -> (('cap, 'a) output -> unit) -> unit
  (**
     Register a function to be triggered just before an output is closed.
  *)

val create_in :
  read:(unit -> char) ->
  input:(string -> int -> int -> int) ->
  close:(unit -> unit) -> [`Read] input
(** Fully create an input by giving all the needed functions.

    {b Note} Do {e not} use this function for creating an input
    which reads from one or more underlying inputs. Rather, use
    {!wrap_in}.
*)

val create_in_seekable :
  read:(unit -> char) ->
  input:(string -> int -> int -> int) ->
  close:(unit -> unit) ->
  seek:(int -> unit) -> [`Read | `Seek] input
(** Fully create a seekable input by giving all the needed functions.
*)

val inherit_in:
  ?read:(unit -> char) ->
  ?input:(string -> int -> int -> int) ->
  ?close:(unit -> unit) ->
  _ input -> [`Read] input
(**
   Simplified and optimized version of {!wrap_in} whenever only
   one input appears as dependency.
*)


val wrap_in :
  read:(unit -> char) ->
  input:(string -> int -> int -> int) ->
  close:(unit -> unit) ->
  underlying:(_ input list) ->
  [`Read] input
(** Fully create an input reading from other inputs by giving all the needed functions.

    This function is a more general version of {!create_in}
    which also handles dependency management between inputs.
*)




val create_out :
  write:(char -> unit) ->
  output:(string -> int -> int -> int) ->
  flush:(unit -> unit) ->
  close:(unit -> 'a) ->
  ([`Write], 'a) output
(**
    Fully create an output by giving all the needed functions.

    @param write  Write one character to the output (see {!write}).
    @param output Write a (sub)string to the output (see {!output}).
    @param flush  Flush any buffers of this output  (see {!flush}).
    @param close  Close this output. The output will be automatically
    flushed.

    {b Note} Do {e not} use this function for creating an output which
    writes to one or more underlying outputs. Rather, use {!wrap_out}.
*)

val create_out_seekable :
  write:(char -> unit) ->
  output:(string -> int -> int -> int) ->
  flush:(unit -> unit) ->
  close:(unit -> 'a) ->
  seek:(int -> unit) ->
  ([`Write | `Seek], 'a) output
(**
   Fully create a seekable output by giving all the needed functions.
   See [create_out].
*)

val inherit_out:
  ?write:(char -> unit) ->
  ?output:(string -> int -> int -> int) ->
  ?flush:(unit -> unit) ->
  ?close:(unit -> unit) ->
  (_, _) output -> ([`Write], unit) output
(**
   Simplified and optimized version of {!wrap_out} whenever only
   one output appears as dependency.
*)


val wrap_out :
  write:(char -> unit)         ->
  output:(string -> int -> int -> int) ->
  flush:(unit -> unit)         ->
  close:(unit -> 'a)           ->
  underlying:((_, 'b) output list)  ->
  ([`Write], 'a) output
(**
   Fully create an output that writes to one or more underlying outputs.

   This function is a more general version of {!create_out},
   which also handles dependency management between outputs.

   To illustrate the need for dependency management, let us consider
   the following values:
   - an output [out]
   - a function [f : _ output -> _ output], using {!create_out} to
   create a new output for writing some data to an underyling
   output (for instance, a function comparale to {!tab_out} or a
   function performing transparent compression or transparent
   traduction between encodings)

   With these values, let us consider the following scenario
   - a new output [f out] is created
   - some data is written to [f out] but not flushed
   - output [out] is closed, perhaps manually or as a consequence
   of garbage-collection, or because the program has ended
   - data written to [f out] is flushed.

   In this case, data reaches [out] only after [out] has been closed,
   which violates the protocol.  Despite appearances, it is quite easy
   to reach such situation, especially in short programs.

   The solution is to use [wrap_out] rather than [create_out] in [f].
   Specifying that [f out] writes on [out] will then let the run-time
   flush and close [f out] when [out] is closed for any reason, which
   in turn avoids the issue.

   @param write  Write one character to the output (see {!write}).
   @param output Write a (sub)string to the output (see {!output}).
   @param flush  Flush any buffers of this output  (see {!flush}).
   @param close  Close this output. The output will be automatically
   flushed.
   @param underlying The list of outputs to which the new output will
   write.

   {b Note} Function [close] should {e not} close [underlying]
   yourself. This is a common mistake which may cause sockets or
   standard output to be closed while they are still being used by
   another part of the program.
*)

val default_buffer_size : int
  (**The default size of buffers.*)


(** {6 Binary files API}

	Here is some API useful for working with binary files, in particular
	binary files generated by C applications. By default, encoding of
	multibyte integers is low-endian. The BigEndian module provide multibyte
	operations with other encoding.
*)

exception Overflow of string
(** Exception raised when a read or write operation cannot be completed. *)

val read_byte : _ input -> int
(** Read an unsigned 8-bit integer. *)

val read_signed_byte : _ input -> int
(** Read an signed 8-bit integer. *)

val read_ui16 : _ input -> int
(** Read an unsigned 16-bit word. *)

val read_i16 : _ input -> int
(** Read a signed 16-bit word. *)

val read_i32 : _ input -> int
(** Read a signed 32-bit integer. Raise [Overflow] if the
  read integer cannot be represented as a Caml 31-bit integer. *)

val read_real_i32 : _ input -> int32
(** Read a signed 32-bit integer as an OCaml int32. *)

val read_i64 : _ input -> int64
(** Read a signed 64-bit integer as an OCaml int64. *)

val read_float : _ input -> float
(** Read an IEEE single precision floating point value. *)

val read_double : _ input -> float
(** Read an IEEE double precision floating point value. *)

val read_string : _ input -> string
(** Read a null-terminated string. *)

val read_line : _ input -> string
(** Read a LF or CRLF terminated string. *)

val write_byte : (_, 'a) output -> int -> unit
(** Write an unsigned 8-bit byte. *)

val write_ui16 : (_, 'a) output -> int -> unit
(** Write an unsigned 16-bit word. *)

val write_i16 : (_, 'a) output -> int -> unit
(** Write a signed 16-bit word. *)

val write_i32 : (_, 'a) output -> int -> unit
(** Write a signed 32-bit integer. *)

val write_real_i32 : (_, 'a) output -> int32 -> unit
(** Write an OCaml int32. *)

val write_i64 : (_, 'a) output -> int64 -> unit
(** Write an OCaml int64. *)

val write_double : (_, 'a) output -> float -> unit
(** Write an IEEE double precision floating point value. *)

val write_float : (_, 'a) output -> float -> unit
(** Write an IEEE single precision floating point value. *)

val write_string : (_, 'a) output -> string -> unit
(** Write a string and append an null character. *)

val write_line : (_, 'a) output -> string -> unit
(** Write a line and append a LF (it might be converted
	to CRLF on some systems depending on the underlying BatIO). *)

external cast_output : (_, 'a) output -> ([`Write], unit) output = "%identity"
(** You can safely transform any output to an unit output in a safe way
  by using this function. *)

(**
   {6 For compatibility purposes}
*)

val input_channel : ?autoclose:bool -> ?cleanup:bool -> in_channel -> [`Read] input
(** Create an input that will read from a channel.

    @param autoclose If true or unspecified, the {!type: input}
    will be automatically closed when the underlying [in_channel]
    has reached its end.

    @param cleanup If true, the channel
    will be automatically closed when the {!type: input} is closed.
    Otherwise, you will need to close the channel manually.
*)

val input_channel_seekable : ?autoclose:bool -> ?cleanup:bool -> in_channel -> [`Read | `Seek] input
(** Create an input that reads from a channel which admits seeking.
    See [input_channel]. *)

val output_channel : ?cleanup:bool -> out_channel -> ([`Write], unit) output
(** Create an output that will write into a channel.

    @param cleanup If true, the channel
    will be automatically closed when the {!type: output} is closed.
    Otherwise, you will need to close the channel manually.
*)

val output_channel_seekable : ?cleanup:bool -> out_channel -> ([`Write | `Seek], unit) output
(** Create an output that will write into a channel and admits seeking.
    See [output_channel].
*)
(*
val to_input_channel : _ input -> in_channel
(** Create a channel that will read from an input.

    {b Note} This function is very costly and is provided
    essentially for debugging purposes or for reusing legacy
    libraries which can't be adapted. As a general rule, if
    you can avoid using this function, don't use it.*)

val to_output_channel: _ output -> out_channel
(** Create a channel that will write to an output

    {b Note} This function is very costly and is provided
    essentially for debugging purposes or for reusing legacy
    libraries which can't be adapted. As a general rule, if
    you can avoid using this function, don't use it.*)
*)
(** {6 Standard inputs/outputs} *)

val stdin : [`Read] input
(** Standard input, as per Unix/Windows conventions (by default, keyboard).*)

val stdout: ([`Write], unit) output
(** Standard output, as per Unix/Windows conventions (by default, console).

    Use this output to display regular messages.*)

val stderr: ([`Write], unit) output
(** Standard error output, as per Unix/Windows conventions.

    Use this output to display warnings and error messages.
*)

val stdnull: ([`Write], unit) output
(** An output which discards everything written to it.

    Use this output to ignore messages.*)

(** {6 Comparison}

    The following modules may be useful to create hashtables of inputs or outputs.
*)



module Input :
sig
  type t = [`Read] input
  val compare : _ input -> _ input -> int
    (**A total order on inputs*)

  val hash    : _ input -> int
    (**A hash function for inputs*)

  val equal : _ input -> _ input -> bool
end

module Output :
sig
  type t = ([`Write], unit) output
  val compare : (_, _) output -> (_, _) output -> int
    (**A total order on outputs*)

  val hash    : (_, _) output -> int
    (**A hash function for outputs*)

  val equal : (_, _) output -> (_, _) output -> bool
end




(**/**)
(**{6 Internals}*)

external noop        : unit      -> unit        = "%ignore"

(**
   {7 Optimized access to fields}
*)

val get_output : (_, _) output -> (string -> int -> int -> int)
val get_flush  : (_, _) output -> (unit -> unit)

val lock : BatConcurrent.lock ref
(**
   A reference to a set of locking operations.
*)


(**
   {7 Facilities for debugging}
*)

val get_output_id : (_, _) output -> int
val get_input_id  : _ input -> int

(**/**)
