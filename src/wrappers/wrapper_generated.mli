(* THIS FILE IS AUTOMATICALLY GENERATED, DO NOT EDIT! *)

type array_
type array_builder
type binary_array
type binary_array_builder
type binary_data_type
type boolean_array
type boolean_array_builder
type boolean_data_type
type buffer
type buffer_input_stream
type buffer_output_stream
type csv_read_options
type csv_reader
type cast_options
type chunked_array
type codec
type column
type compressed_input_stream
type compressed_output_stream
type count_options
type data_type
type date32_array
type date32_array_builder
type date32_data_type
type date64_array
type date64_array_builder
type date64_data_type
type decimal128
type decimal128_array
type decimal128_array_builder
type decimal128_data_type
type decimal_data_type
type dense_union_array
type dense_union_data_type
type dictionary_array
type dictionary_data_type
type double_array
type double_array_builder
type double_data_type
type feather_file_reader
type feather_file_writer
type field
type file_output_stream
type fixed_size_binary_array
type fixed_size_binary_data_type
type fixed_width_data_type
type float_array
type float_array_builder
type float_data_type
type floating_point_data_type
type gio_input_stream
type gio_output_stream
type input_stream
type int16_array
type int16_array_builder
type int16_data_type
type int32_array
type int32_array_builder
type int32_data_type
type int64_array
type int64_array_builder
type int64_data_type
type int8_array
type int8_array_builder
type int8_data_type
type int_array_builder
type integer_data_type
type list_array
type list_array_builder
type list_data_type
type memory_mapped_input_stream
type mutable_buffer
type null_array
type null_array_builder
type null_data_type
type numeric_array
type numeric_data_type
type output_stream
type primitive_array
type record_batch
type record_batch_builder
type record_batch_file_reader
type record_batch_file_writer
type record_batch_reader
type record_batch_stream_reader
type record_batch_stream_writer
type record_batch_writer
type resizable_buffer
type schema
type seekable_input_stream
type sparse_union_array
type sparse_union_data_type
type string_array
type string_array_builder
type string_data_type
type struct_array
type struct_array_builder
type struct_data_type
type table
type table_batch_reader
type tensor
type time32_array
type time32_array_builder
type time32_data_type
type time64_array
type time64_array_builder
type time64_data_type
type time_data_type
type timestamp_array
type timestamp_array_builder
type timestamp_data_type
type u_int16_array
type u_int16_array_builder
type u_int16_data_type
type u_int32_array
type u_int32_array_builder
type u_int32_data_type
type u_int64_array
type u_int64_array_builder
type u_int64_data_type
type u_int8_array
type u_int8_array_builder
type u_int8_data_type
type u_int_array_builder
type union_array
type union_data_type

module Array : sig
  type t = array_
  val cast : t -> data_type -> cast_options -> array_
  val count : t -> count_options -> Int64.t
  val count_values : t -> struct_array
  val dictionary_encode : t -> dictionary_array
  val equal : t -> array_ -> bool
  val equal_approx : t -> array_ -> bool
  val equal_range : t -> Int64.t -> array_ -> Int64.t -> Int64.t -> bool
  val get_length : t -> Int64.t
  val get_n_nulls : t -> Int64.t
  val get_null_bitmap : t -> buffer
  val get_offset : t -> Int64.t
  val get_value_data_type : t -> data_type
  val is_null : t -> Int64.t -> bool
  val is_valid : t -> Int64.t -> bool
  val slice : t -> Int64.t -> Int64.t -> array_
  val to_string : t -> string
  val unique : t -> array_
end

module ArrayBuilder : sig
  type t = array_builder
  val finish : t -> array_
  val get_value_data_type : t -> data_type
end

module BinaryArray : sig
  type t = binary_array
  val new_ : Int64.t -> buffer -> buffer -> buffer -> Int64.t -> t
  val get_buffer : t -> buffer
  val get_offsets_buffer : t -> buffer
end

module BinaryArrayBuilder : sig
  type t = binary_array_builder
  val new_ : unit -> t
  val append_null : t -> bool
end

module BinaryDataType : sig
  type t = binary_data_type
  val new_ : unit -> t
end

module BooleanArray : sig
  type t = boolean_array
  val new_ : Int64.t -> buffer -> buffer -> Int64.t -> t
  val and_ : t -> boolean_array -> boolean_array
  val get_value : t -> Int64.t -> bool
  val invert : t -> boolean_array
  val or_ : t -> boolean_array -> boolean_array
  val xor : t -> boolean_array -> boolean_array
end

module BooleanArrayBuilder : sig
  type t = boolean_array_builder
  val new_ : unit -> t
  val append_null : t -> bool
  val append_nulls : t -> Int64.t -> bool
  val append_value : t -> bool -> bool
end

module BooleanDataType : sig
  type t = boolean_data_type
  val new_ : unit -> t
end

module Buffer : sig
  type t = buffer
  val copy : t -> Int64.t -> Int64.t -> buffer
  val equal : t -> buffer -> bool
  val equal_n_bytes : t -> buffer -> Int64.t -> bool
  val get_capacity : t -> Int64.t
  val get_parent : t -> buffer
  val get_size : t -> Int64.t
  val is_mutable : t -> bool
  val slice : t -> Int64.t -> Int64.t -> buffer
end

module BufferInputStream : sig
  type t = buffer_input_stream
  val new_ : buffer -> t
  val get_buffer : t -> buffer
end

module BufferOutputStream : sig
  type t = buffer_output_stream
  val new_ : resizable_buffer -> t
end

module CSVReadOptions : sig
  type t = csv_read_options
  val new_ : unit -> t
end

module CSVReader : sig
  type t = csv_reader
  val new_ : input_stream -> csv_read_options -> t
  val read : t -> table
end

module CastOptions : sig
  type t = cast_options
  val new_ : unit -> t
end

module ChunkedArray : sig
  type t = chunked_array
  val equal : t -> chunked_array -> bool
  val get_value_data_type : t -> data_type
  val to_string : t -> string
end

module Codec : sig
  type t = codec
  val get_name : t -> string
end

module Column : sig
  type t = column
  val new_array : field -> array_ -> t
  val new_chunked_array : field -> chunked_array -> t
  val equal : t -> column -> bool
  val get_data : t -> chunked_array
  val get_data_type : t -> data_type
  val get_field : t -> field
  val get_name : t -> string
  val to_string : t -> string
end

module CompressedInputStream : sig
  type t = compressed_input_stream
  val new_ : codec -> input_stream -> t
end

module CompressedOutputStream : sig
  type t = compressed_output_stream
  val new_ : codec -> output_stream -> t
end

module CountOptions : sig
  type t = count_options
  val new_ : unit -> t
end

module DataType : sig
  type t = data_type
  val equal : t -> data_type -> bool
  val to_string : t -> string
end

module Date32Array : sig
  type t = date32_array
  val new_ : Int64.t -> buffer -> buffer -> Int64.t -> t
end

module Date32ArrayBuilder : sig
  type t = date32_array_builder
  val new_ : unit -> t
  val append_null : t -> bool
  val append_nulls : t -> Int64.t -> bool
end

module Date32DataType : sig
  type t = date32_data_type
  val new_ : unit -> t
end

module Date64Array : sig
  type t = date64_array
  val new_ : Int64.t -> buffer -> buffer -> Int64.t -> t
  val get_value : t -> Int64.t -> Int64.t
end

module Date64ArrayBuilder : sig
  type t = date64_array_builder
  val new_ : unit -> t
  val append_null : t -> bool
  val append_nulls : t -> Int64.t -> bool
  val append_value : t -> Int64.t -> bool
end

module Date64DataType : sig
  type t = date64_data_type
  val new_ : unit -> t
end

module Decimal128 : sig
  type t = decimal128
  val new_integer : Int64.t -> t
  val new_string : string -> t
  val divide : t -> decimal128 -> decimal128 -> decimal128
  val equal : t -> decimal128 -> bool
  val greater_than : t -> decimal128 -> bool
  val greater_than_or_equal : t -> decimal128 -> bool
  val less_than : t -> decimal128 -> bool
  val less_than_or_equal : t -> decimal128 -> bool
  val minus : t -> decimal128 -> decimal128
  val multiply : t -> decimal128 -> decimal128
  val not_equal : t -> decimal128 -> bool
  val plus : t -> decimal128 -> decimal128
  val to_integer : t -> Int64.t
  val to_string : t -> string
end

module Decimal128Array : sig
  type t = decimal128_array
  val format_value : t -> Int64.t -> string
  val get_value : t -> Int64.t -> decimal128
end

module Decimal128ArrayBuilder : sig
  type t = decimal128_array_builder
  val new_ : decimal128_data_type -> t
  val append_null : t -> bool
  val append_value : t -> decimal128 -> bool
end

module Decimal128DataType : sig
  type t = decimal128_data_type
end

module DecimalDataType : sig
  type t = decimal_data_type
end

module DenseUnionArray : sig
  type t = dense_union_array
end

module DenseUnionDataType : sig
  type t = dense_union_data_type
end

module DictionaryArray : sig
  type t = dictionary_array
  val new_ : data_type -> array_ -> t
  val get_dictionary : t -> array_
  val get_dictionary_data_type : t -> dictionary_data_type
  val get_indices : t -> array_
end

module DictionaryDataType : sig
  type t = dictionary_data_type
  val new_ : data_type -> array_ -> bool -> t
  val get_dictionary : t -> array_
  val get_index_data_type : t -> data_type
  val is_ordered : t -> bool
end

module DoubleArray : sig
  type t = double_array
  val new_ : Int64.t -> buffer -> buffer -> Int64.t -> t
  val get_value : t -> Int64.t -> float
  val sum : t -> float
end

module DoubleArrayBuilder : sig
  type t = double_array_builder
  val new_ : unit -> t
  val append_null : t -> bool
  val append_nulls : t -> Int64.t -> bool
  val append_value : t -> float -> bool
end

module DoubleDataType : sig
  type t = double_data_type
  val new_ : unit -> t
end

module FeatherFileReader : sig
  type t = feather_file_reader
  val new_ : seekable_input_stream -> t
  val get_description : t -> string
  val get_n_columns : t -> Int64.t
  val get_n_rows : t -> Int64.t
  val has_description : t -> bool
  val read : t -> table
end

module FeatherFileWriter : sig
  type t = feather_file_writer
  val new_ : output_stream -> t
  val append : t -> string -> array_ -> bool
  val close : t -> bool
  val write : t -> table -> bool
end

module Field : sig
  type t = field
  val new_ : string -> data_type -> t
  val new_full : string -> data_type -> bool -> t
  val equal : t -> field -> bool
  val get_data_type : t -> data_type
  val get_name : t -> string
  val is_nullable : t -> bool
  val to_string : t -> string
end

module FileOutputStream : sig
  type t = file_output_stream
  val new_ : string -> bool -> t
end

module FixedSizeBinaryArray : sig
  type t = fixed_size_binary_array
end

module FixedSizeBinaryDataType : sig
  type t = fixed_size_binary_data_type
end

module FixedWidthDataType : sig
  type t = fixed_width_data_type
end

module FloatArray : sig
  type t = float_array
  val new_ : Int64.t -> buffer -> buffer -> Int64.t -> t
  val get_value : t -> Int64.t -> float
  val sum : t -> float
end

module FloatArrayBuilder : sig
  type t = float_array_builder
  val new_ : unit -> t
  val append_null : t -> bool
  val append_nulls : t -> Int64.t -> bool
  val append_value : t -> float -> bool
end

module FloatDataType : sig
  type t = float_data_type
  val new_ : unit -> t
end

module FloatingPointDataType : sig
  type t = floating_point_data_type
end

module GIOInputStream : sig
  type t = gio_input_stream
  val new_ : input_stream -> t
end

module GIOOutputStream : sig
  type t = gio_output_stream
  val new_ : output_stream -> t
end

module InputStream : sig
  type t = input_stream
  val advance : t -> Int64.t -> bool
  val read_tensor : t -> tensor
end

module Int16Array : sig
  type t = int16_array
  val new_ : Int64.t -> buffer -> buffer -> Int64.t -> t
  val sum : t -> Int64.t
end

module Int16ArrayBuilder : sig
  type t = int16_array_builder
  val new_ : unit -> t
  val append_null : t -> bool
  val append_nulls : t -> Int64.t -> bool
end

module Int16DataType : sig
  type t = int16_data_type
  val new_ : unit -> t
end

module Int32Array : sig
  type t = int32_array
  val new_ : Int64.t -> buffer -> buffer -> Int64.t -> t
  val sum : t -> Int64.t
end

module Int32ArrayBuilder : sig
  type t = int32_array_builder
  val new_ : unit -> t
  val append_null : t -> bool
  val append_nulls : t -> Int64.t -> bool
end

module Int32DataType : sig
  type t = int32_data_type
  val new_ : unit -> t
end

module Int64Array : sig
  type t = int64_array
  val new_ : Int64.t -> buffer -> buffer -> Int64.t -> t
  val get_value : t -> Int64.t -> Int64.t
  val sum : t -> Int64.t
end

module Int64ArrayBuilder : sig
  type t = int64_array_builder
  val new_ : unit -> t
  val append_null : t -> bool
  val append_nulls : t -> Int64.t -> bool
  val append_value : t -> Int64.t -> bool
end

module Int64DataType : sig
  type t = int64_data_type
  val new_ : unit -> t
end

module Int8Array : sig
  type t = int8_array
  val new_ : Int64.t -> buffer -> buffer -> Int64.t -> t
  val sum : t -> Int64.t
end

module Int8ArrayBuilder : sig
  type t = int8_array_builder
  val new_ : unit -> t
  val append_null : t -> bool
  val append_nulls : t -> Int64.t -> bool
end

module Int8DataType : sig
  type t = int8_data_type
  val new_ : unit -> t
end

module IntArrayBuilder : sig
  type t = int_array_builder
  val new_ : unit -> t
  val append_null : t -> bool
  val append_nulls : t -> Int64.t -> bool
  val append_value : t -> Int64.t -> bool
end

module IntegerDataType : sig
  type t = integer_data_type
end

module ListArray : sig
  type t = list_array
  val new_ : data_type -> Int64.t -> buffer -> array_ -> buffer -> Int64.t -> t
  val get_value : t -> Int64.t -> array_
  val get_value_type : t -> data_type
end

module ListArrayBuilder : sig
  type t = list_array_builder
  val new_ : list_data_type -> t
  val append_null : t -> bool
  val append_value : t -> bool
  val get_value_builder : t -> array_builder
end

module ListDataType : sig
  type t = list_data_type
  val new_ : field -> t
  val get_field : t -> field
end

module MemoryMappedInputStream : sig
  type t = memory_mapped_input_stream
  val new_ : string -> t
end

module MutableBuffer : sig
  type t = mutable_buffer
  val slice : t -> Int64.t -> Int64.t -> mutable_buffer
end

module NullArray : sig
  type t = null_array
  val new_ : Int64.t -> t
end

module NullArrayBuilder : sig
  type t = null_array_builder
  val new_ : unit -> t
  val append_null : t -> bool
  val append_nulls : t -> Int64.t -> bool
end

module NullDataType : sig
  type t = null_data_type
  val new_ : unit -> t
end

module NumericArray : sig
  type t = numeric_array
  val mean : t -> float
end

module NumericDataType : sig
  type t = numeric_data_type
end

module OutputStream : sig
  type t = output_stream
  val write_tensor : t -> tensor -> Int64.t
end

module PrimitiveArray : sig
  type t = primitive_array
  val get_buffer : t -> buffer
end

module RecordBatch : sig
  type t = record_batch
  val equal : t -> record_batch -> bool
  val get_n_rows : t -> Int64.t
  val get_schema : t -> schema
  val slice : t -> Int64.t -> Int64.t -> record_batch
  val to_string : t -> string
end

module RecordBatchBuilder : sig
  type t = record_batch_builder
  val new_ : schema -> t
  val flush : t -> record_batch
  val get_initial_capacity : t -> Int64.t
  val get_schema : t -> schema
end

module RecordBatchFileReader : sig
  type t = record_batch_file_reader
  val new_ : seekable_input_stream -> t
  val get_schema : t -> schema
end

module RecordBatchFileWriter : sig
  type t = record_batch_file_writer
  val new_ : output_stream -> schema -> t
end

module RecordBatchReader : sig
  type t = record_batch_reader
  val get_schema : t -> schema
  val read_next : t -> record_batch
end

module RecordBatchStreamReader : sig
  type t = record_batch_stream_reader
  val new_ : input_stream -> t
end

module RecordBatchStreamWriter : sig
  type t = record_batch_stream_writer
  val new_ : output_stream -> schema -> t
end

module RecordBatchWriter : sig
  type t = record_batch_writer
  val close : t -> bool
  val write_record_batch : t -> record_batch -> bool
  val write_table : t -> table -> bool
end

module ResizableBuffer : sig
  type t = resizable_buffer
  val new_ : Int64.t -> t
  val reserve : t -> Int64.t -> bool
  val resize : t -> Int64.t -> bool
end

module Schema : sig
  type t = schema
  val equal : t -> schema -> bool
  val get_field_by_name : t -> string -> field
  val to_string : t -> string
end

module SeekableInputStream : sig
  type t = seekable_input_stream
  val get_support_zero_copy : t -> bool
  val read_at : t -> Int64.t -> Int64.t -> buffer
end

module SparseUnionArray : sig
  type t = sparse_union_array
end

module SparseUnionDataType : sig
  type t = sparse_union_data_type
end

module StringArray : sig
  type t = string_array
  val new_ : Int64.t -> buffer -> buffer -> buffer -> Int64.t -> t
  val get_string : t -> Int64.t -> string
end

module StringArrayBuilder : sig
  type t = string_array_builder
  val new_ : unit -> t
  val append_value : t -> string -> bool
end

module StringDataType : sig
  type t = string_data_type
  val new_ : unit -> t
end

module StructArray : sig
  type t = struct_array
end

module StructArrayBuilder : sig
  type t = struct_array_builder
  val new_ : struct_data_type -> t
  val append_null : t -> bool
  val append_value : t -> bool
end

module StructDataType : sig
  type t = struct_data_type
  val get_field_by_name : t -> string -> field
end

module Table : sig
  type t = table
  val equal : t -> table -> bool
  val get_schema : t -> schema
  val to_string : t -> string
end

module TableBatchReader : sig
  type t = table_batch_reader
  val new_ : table -> t
end

module Tensor : sig
  type t = tensor
  val equal : t -> tensor -> bool
  val get_buffer : t -> buffer
  val get_size : t -> Int64.t
  val get_value_data_type : t -> data_type
  val is_column_major : t -> bool
  val is_contiguous : t -> bool
  val is_mutable : t -> bool
  val is_row_major : t -> bool
end

module Time32Array : sig
  type t = time32_array
  val new_ : time32_data_type -> Int64.t -> buffer -> buffer -> Int64.t -> t
end

module Time32ArrayBuilder : sig
  type t = time32_array_builder
  val new_ : time32_data_type -> t
  val append_null : t -> bool
  val append_nulls : t -> Int64.t -> bool
end

module Time32DataType : sig
  type t = time32_data_type
end

module Time64Array : sig
  type t = time64_array
  val new_ : time64_data_type -> Int64.t -> buffer -> buffer -> Int64.t -> t
  val get_value : t -> Int64.t -> Int64.t
end

module Time64ArrayBuilder : sig
  type t = time64_array_builder
  val new_ : time64_data_type -> t
  val append_null : t -> bool
  val append_nulls : t -> Int64.t -> bool
  val append_value : t -> Int64.t -> bool
end

module Time64DataType : sig
  type t = time64_data_type
end

module TimeDataType : sig
  type t = time_data_type
end

module TimestampArray : sig
  type t = timestamp_array
  val new_ : timestamp_data_type -> Int64.t -> buffer -> buffer -> Int64.t -> t
  val get_value : t -> Int64.t -> Int64.t
end

module TimestampArrayBuilder : sig
  type t = timestamp_array_builder
  val new_ : timestamp_data_type -> t
  val append_null : t -> bool
  val append_nulls : t -> Int64.t -> bool
  val append_value : t -> Int64.t -> bool
end

module TimestampDataType : sig
  type t = timestamp_data_type
end

module UInt16Array : sig
  type t = u_int16_array
  val new_ : Int64.t -> buffer -> buffer -> Int64.t -> t
end

module UInt16ArrayBuilder : sig
  type t = u_int16_array_builder
  val new_ : unit -> t
  val append_null : t -> bool
  val append_nulls : t -> Int64.t -> bool
end

module UInt16DataType : sig
  type t = u_int16_data_type
  val new_ : unit -> t
end

module UInt32Array : sig
  type t = u_int32_array
  val new_ : Int64.t -> buffer -> buffer -> Int64.t -> t
end

module UInt32ArrayBuilder : sig
  type t = u_int32_array_builder
  val new_ : unit -> t
  val append_null : t -> bool
  val append_nulls : t -> Int64.t -> bool
end

module UInt32DataType : sig
  type t = u_int32_data_type
  val new_ : unit -> t
end

module UInt64Array : sig
  type t = u_int64_array
  val new_ : Int64.t -> buffer -> buffer -> Int64.t -> t
end

module UInt64ArrayBuilder : sig
  type t = u_int64_array_builder
  val new_ : unit -> t
  val append_null : t -> bool
  val append_nulls : t -> Int64.t -> bool
end

module UInt64DataType : sig
  type t = u_int64_data_type
  val new_ : unit -> t
end

module UInt8Array : sig
  type t = u_int8_array
  val new_ : Int64.t -> buffer -> buffer -> Int64.t -> t
end

module UInt8ArrayBuilder : sig
  type t = u_int8_array_builder
  val new_ : unit -> t
  val append_null : t -> bool
  val append_nulls : t -> Int64.t -> bool
end

module UInt8DataType : sig
  type t = u_int8_data_type
  val new_ : unit -> t
end

module UIntArrayBuilder : sig
  type t = u_int_array_builder
  val new_ : unit -> t
  val append_null : t -> bool
  val append_nulls : t -> Int64.t -> bool
end

module UnionArray : sig
  type t = union_array
end

module UnionDataType : sig
  type t = union_data_type
end
