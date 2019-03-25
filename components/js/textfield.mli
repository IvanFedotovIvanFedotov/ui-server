open Js_of_ocaml

module Event : sig
  class type icon =
    object
      inherit [unit] Widget.custom_event
    end

  val icon : icon Js.t Events.Typ.t
end

module Icon : sig
  class type t =
    object
      inherit Widget.t

      (** Updates the icon's aria-label. *)
      method set_aria_label : string -> unit

      (** Updates the icon's disabled state. *)
      method set_disabled : bool -> unit

      (** Private methods *)

      (** Emits a custom event "textfield:icon" denoting a user has clicked
          the icon, which bubbles to the top-level text field element. *)
      method private notify_action : unit -> unit

      method private handle_keydown : Dom_html.keyboardEvent Js.t -> unit Lwt.t

      method private handle_click : Dom_html.mouseEvent Js.t -> unit Lwt.t
    end

  val attach : #Dom_html.element Js.t -> t
end

module Helper_text : sig
  class type t =
    object
      inherit Widget.t

      (** Sets the content of the helper text field. *)
      method set_content : string -> unit

      (** Sets the persistency of the helper text. *)
      method set_persistent : bool -> unit

      (** [true] makes the helper text act as an error validation message. *)
      method set_validation : bool -> unit

      (** Makes the helper text visible to the screen reader. *)
      method show_to_screen_reader : unit -> unit

      (** Sets the validity of the helper text based on the input validity. *)
      method set_validity : bool -> unit

      (** Private methods *)

      (** Hides the help text from screen readers. *)
      method private hide : unit -> unit
    end

  val make : ?persistent:bool -> ?validation:bool -> string -> t

  val attach : #Dom_html.element Js.t -> t
end
