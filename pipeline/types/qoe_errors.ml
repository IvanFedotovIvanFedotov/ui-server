open Application_types

include Qoe_backend_types__Qoe_errors.Make (Stream.ID) (Time.Useconds) (Time.Period.Useconds)
   
let name = "errors"
