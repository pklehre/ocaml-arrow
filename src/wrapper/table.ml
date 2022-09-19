open Base
include Wrapper.Table

type _ col_type =
  | Int : int col_type
  | Float : float col_type
  | Utf8 : string col_type
  | Date : Core.Date.t col_type
  | Time_ns : Core.Time_ns_unix.of_string)
  | Span_ns ->
    (try Wrapper.Column.read_span_ns t ~column with
    | _ ->
      Wrapper.Column.read_utf8 t ~column
      |> Array.map ~f:Core.Time_ns.Span.of_string)
  | Ofday_ns ->
    (try Wrapper.Column.read_ofday_ns t ~column with
    | _ ->
      Wrapper.Column.read_utf8 t ~column
      |> Array.map ~f:Core.Time_ns.Ofday.of_string)
  | Bool ->
    let bs = Wrapper.Column.read_bitset t ~column in
    Array.init (Valid.length bs) ~f:(Valid.get bs)

let read_opt (type a) t ~column (col_type : a col_type) : a option array =
  match col_type with
  | Int -> Wrapper.Column.read_int_opt t ~column
  | Float -> Wrapper.Column.read_float_opt t ~column
  | Utf8 -> Wrapper.Column.read_utf8_opt t ~column
  | Date ->
    (try Wrapper.Column.read_date_opt t ~column with
    | _ ->
      Wrapper.Column.read_utf8_opt t ~column
      |> Array.map ~f:(Option.map ~f:Core.Date.of_string))
  | Time_ns ->
    (try Wrapper.Column.read_time_ns_opt t ~column with
    | _ ->
      Wrapper.Column.read_utf8_opt t ~column
      |> Array.map ~f:(Option.map ~f:Time_ns_unix.of_string))
  | Span_ns ->
    (try Wrapper.Column.read_span_ns_opt t ~column with
    | _ ->
      Wrapper.Column.read_utf8_opt t ~column
      |> Array.map ~f:(Option.map ~f:Core.Time_ns.Span.of_string))
  | Ofday_ns ->
    (try Wrapper.Column.read_ofday_ns_opt t ~column with
    | _ ->
      Wrapper.Column.read_utf8_opt t ~column
      |> Array.map ~f:(Option.map ~f:Core.Time_ns.Ofday.of_string))
  | Bool ->
    let bs, valid = Wrapper.Column.read_bitset_opt t ~column in
    Array.init (Valid.length bs) ~f:(fun i ->
        if Valid.get valid i then Valid.get bs i |> Option.some else None)
