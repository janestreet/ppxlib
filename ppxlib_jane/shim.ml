open Stdppx
open Ppxlib_ast.Asttypes
open Ppxlib_ast.Parsetree

type mode = Mode of string [@@unboxed]

type arrow_argument =
  { arg_label : arg_label
  ; arg_modes : mode list
  ; arg_type : core_type
  }

type arrow_result =
  { result_modes : mode list
  ; result_type : core_type
  }

type nonrec modality = modality = Modality of string [@@unboxed]

module Modality = struct
  let to_ast_modalities_list ~loc modalities =
    List.map modalities ~f:(fun modality -> { txt = modality; loc })
  ;;

  let of_ast_modalities_list ast_modalities =
    List.map ast_modalities ~f:(fun { txt = modality; _ } -> modality)
  ;;
end

module Pcstr_tuple_arg = struct
  type t = constructor_argument

  let extract_modalities t = Modality.of_ast_modalities_list t.pca_modalities, t.pca_type
  let to_core_type t = t.pca_type

  let of_core_type core_type =
    { pca_type = core_type; pca_loc = core_type.ptyp_loc; pca_modalities = [] }
  ;;

  let map_core_type t ~f = { t with pca_type = f t.pca_type }

  let map_core_type_extra t ~f =
    let pca_type, extra = f t.pca_type in
    { t with pca_type }, extra
  ;;

  let create ~loc ~modalities ~type_ =
    { pca_type = type_
    ; pca_loc = loc
    ; pca_modalities = Modality.to_ast_modalities_list ~loc modalities
    }
  ;;
end

module Label_declaration = struct
  let extract_modalities ld =
    Modality.of_ast_modalities_list ld.pld_modalities, { ld with pld_modalities = [] }
  ;;

  let create ~loc ~name ~mutable_ ~modalities ~type_ =
    { pld_loc = loc
    ; pld_modalities = Modality.to_ast_modalities_list ~loc modalities
    ; pld_name = name
    ; pld_type = type_
    ; pld_mutable = mutable_
    ; pld_attributes = []
    }
  ;;
end

module Value_description = struct
  let extract_modalities vd =
    Modality.of_ast_modalities_list vd.pval_modalities, { vd with pval_modalities = [] }
  ;;

  let create ~loc ~name ~type_ ~modalities ~prim =
    { pval_loc = loc
    ; pval_modalities = Modality.to_ast_modalities_list ~loc modalities
    ; pval_name = name
    ; pval_type = type_
    ; pval_prim = prim
    ; pval_attributes = []
    }
  ;;
end

module Pexp_function = struct
  type nonrec function_param_desc = function_param_desc =
    | Pparam_val of arg_label * expression option * pattern
    | Pparam_newtype of string loc * jkind_annotation loc option

  type nonrec function_param = function_param =
    { pparam_loc : Location.t
    ; pparam_desc : function_param_desc
    }

  type nonrec function_constraint = function_constraint =
    { mode_annotations : mode_expression
    ; type_constraint : type_constraint
    }

  type nonrec type_constraint = type_constraint =
    | Pconstraint of core_type
    | Pcoerce of core_type option * core_type

  type nonrec function_body = function_body =
    | Pfunction_body of expression
    | Pfunction_cases of case list * Location.t * attributes

  let to_parsetree ~params ~constraint_ ~body = Pexp_function (params, constraint_, body)

  (* The ignored [loc] argument is used in shim_upstream.ml. *)
  let of_parsetree expr_desc ~loc:_ =
    match expr_desc with
    | Pexp_function (a, b, c) -> Some (a, b, c)
    | _ -> None
  ;;
end

module Expression_desc = struct
  type t = expression_desc =
    | Pexp_ident of Longident.t loc
    | Pexp_constant of constant
    | Pexp_let of rec_flag * value_binding list * expression
    | Pexp_function of
        Pexp_function.function_param list
        * Pexp_function.function_constraint option
        * Pexp_function.function_body
    | Pexp_apply of expression * (arg_label * expression) list
    | Pexp_match of expression * case list
    | Pexp_try of expression * case list
    | Pexp_tuple of expression list
    | Pexp_construct of Longident.t loc * expression option
    | Pexp_variant of label * expression option
    | Pexp_record of (Longident.t loc * expression) list * expression option
    | Pexp_field of expression * Longident.t loc
    | Pexp_setfield of expression * Longident.t loc * expression
    | Pexp_array of expression list
    | Pexp_ifthenelse of expression * expression * expression option
    | Pexp_sequence of expression * expression
    | Pexp_while of expression * expression
    | Pexp_for of pattern * expression * expression * direction_flag * expression
    | Pexp_constraint of expression * core_type
    | Pexp_coerce of expression * core_type option * core_type
    | Pexp_send of expression * label loc
    | Pexp_new of Longident.t loc
    | Pexp_setinstvar of label loc * expression
    | Pexp_override of (label loc * expression) list
    | Pexp_letmodule of string option loc * module_expr * expression
    | Pexp_letexception of extension_constructor * expression
    | Pexp_assert of expression
    | Pexp_lazy of expression
    | Pexp_poly of expression * core_type option
    | Pexp_object of class_structure
    | Pexp_newtype of string loc * expression
    | Pexp_pack of module_expr
    | Pexp_open of open_declaration * expression
    | Pexp_letop of letop
    | Pexp_extension of extension
    | Pexp_unreachable

  (* The ignored [loc] argument is used in shim_upstream.ml. *)
  let of_parsetree x ~loc:_ = x
  let to_parsetree x = x
end

type nonrec mode_expression = mode_expression
type nonrec mode_const_expression = mode_const_expression
type nonrec jkind_const_annotation = jkind_const_annotation

type nonrec jkind_annotation = jkind_annotation =
  | Default
  | Abbreviation of jkind_const_annotation
  | Mod of jkind_annotation * mode_expression
  | With of jkind_annotation * core_type
  | Kind_of of core_type
