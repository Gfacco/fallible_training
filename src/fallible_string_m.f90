module fallible_string_m  
  use erloff, only: error_list_t, fatal_t, module_t, procedure_t
  use iso_varying_string, only: varying_string, assignment(=)
  use rojff, only: &
      fallible_json_value_t, &
      json_element_t, &
      json_string_t, &
      json_value_t


  implicit none
  private
  public :: fallible_string_t

  type :: fallible_string_t
    !! This type serves to allow a function to return either an string or a list of errors.
    !! For example, this type can ensure that a user input is safe.
    private
    type(varying_string) :: var_string_
    type(error_list_t) :: errors_
  contains
    procedure :: failed
    procedure :: var_string
    procedure :: errors
  end type

  interface fallible_string_t
    module procedure from_json_element
    module procedure from_json_value
    module procedure from_fallible_string
    module procedure from_fallible_json_value
  end interface

  character(len=*), parameter :: MODULE_NAME = "fallible_string_m"
contains
  impure elemental function from_json_element(json) result(fallible_string)
    type(json_element_t), intent(in) :: json
    type(fallible_string_t) :: fallible_string

    fallible_string = fallible_string_t(fallible_string_t(json%json), module_t(MODULE_NAME), procedure_t("from_json_element"))
  end function

  function from_json_value(json) result(fallible_string)
    class(json_value_t), intent(in) :: json
    type(fallible_string_t) :: fallible_string

    select type (json)
    type is (json_string_t)
      fallible_string%var_string_ = json%string
    class default
      fallible_string%errors_ = error_list_t(fatal_t( &
          module_t(MODULE_NAME), &
          procedure_t("from_json_value"), &
          json%to_compact_string() // " was not an json_string_t."))
    end select
  end function

  function from_fallible_string(fallible_string, module_, procedure_) result(new_fallible_string)
    !! This allows a procedure to either continue on with the returned value, or append its name to the call stack of any errors.
    type(fallible_string_t), intent(in) :: fallible_string
    type(module_t), intent(in) :: module_
    type(procedure_t), intent(in) :: procedure_
    type(fallible_string_t) :: new_fallible_string

    if (fallible_string%failed()) then
      new_fallible_string%errors_ = error_list_t(fallible_string%errors_, module_, procedure_)
    else
      new_fallible_string%var_string_ = fallible_string%var_string_
    end if
  end function

  function from_fallible_json_value(json) result(fallible_string)
    type(fallible_json_value_t), intent(in) :: json
    type(fallible_string_t) :: fallible_string

    if (json%failed()) then
      fallible_string%errors_ = json%errors
    else
      fallible_string = fallible_string_t( &
          fallible_string_t(json%value_), &
          module_t(MODULE_NAME), &
          procedure_t("from_fallible_json_value"))
    end if
  end function

  elemental function failed(self)
    class(fallible_string_t), intent(in) :: self
    logical :: failed

      failed = self%errors_%has_any()
  end function

  elemental function var_string(self)
    class(fallible_string_t), intent(in) :: self
    type(varying_string)  :: var_string

    var_string = self%var_string_
  end function

  impure elemental function errors(self)
    class(fallible_string_t), intent(in) :: self
    type(error_list_t) :: errors

    errors = self%errors_
  end function
end module