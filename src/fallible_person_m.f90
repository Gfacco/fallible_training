module fallible_person_m
  use person_m, only: person_t
  use erloff, only: error_list_t, module_t, procedure_t, fatal_t
  use rojff, only: json_value_t, json_object_t, fallible_json_value_t
  use fallible_string_m, only: fallible_string_t

implicit none

public :: fallible_person_t

type :: fallible_person_t
  private
  type(person_t) :: person_
  type(error_list_t) :: errors_ 
contains
  procedure :: failed
  procedure :: person
  procedure :: errors
end type

interface fallible_person_t
  module procedure from_json_value
  module procedure from_errors
  module procedure from_fallible_person
  module procedure from_fallible_json_value
end interface

character(len=*), parameter :: MODULE_NAME = "fallible_person_m"

contains 
function from_json_value(json_value) result(fallible_person)
  implicit none
  class(json_value_t), intent(in) :: json_value
  type(fallible_person_t) :: fallible_person

  character(len=*), parameter :: PROCEDURE_NAME = "from_json_value"

  select type (json_value)
  type is (json_object_t)
      fallible_person = fallible_person_t( &
          from_json_object(json_value), &
          module_t(MODULE_NAME), &
          procedure_t(PROCEDURE_NAME))
  class default
      fallible_person%errors_ = error_list_t(fatal_t( &
          module_t(MODULE_NAME), &
          procedure_t(PROCEDURE_NAME), &
          json_value%to_compact_string() // " was not an object"))
  end select
end function

function from_json_object(json_object) result(fallible_person)
  implicit none
  type(json_object_t), intent(in) :: json_object
  type(fallible_person_t) :: fallible_person
  character(len=*), parameter :: PROCEDURE_NAME = "from_json_object"

  type(fallible_string_t) :: maybe_name
  maybe_name = fallible_string_t(json_object%get("name"))

  if (any( &
      [ maybe_name%failed() &
      ])) then

    fallible_person%errors_ = error_list_t( &
      [ maybe_name%errors() &
      ,  error_list_t(fatal_t( &
        module_t(MODULE_NAME), &
        procedure_t(PROCEDURE_NAME), &
        json_object%to_compact_string() // " not a valid person object")) &
      ], &
      module_t(MODULE_NAME), &
      procedure_t(PROCEDURE_NAME) &
      )
  else
    fallible_person%person_ = person_t( &
            name = maybe_name%var_string())
  end if

end function

function from_fallible_person(fallible_person, module_, procedure_) &
  result(new_fallible_person)
  implicit none
  type(fallible_person_t), intent(in) :: fallible_person
  type(module_t), intent(in) :: module_
  type(procedure_t), intent(in) :: procedure_
  type(fallible_person_t) :: new_fallible_person

  if (fallible_person%failed()) then
    new_fallible_person%errors_ = error_list_t( &
            fallible_person%errors_, module_, procedure_)
  else
    new_fallible_person%person_ = fallible_person%person_
  end if
end function

function from_fallible_json_value(json_value) result(fallible_person)
  type(fallible_json_value_t), intent(in) :: json_value
  type(fallible_person_t) :: fallible_person

  if (json_value%failed()) then
    fallible_person%errors_ = json_value%errors
  else
    fallible_person = fallible_person_t( &
        fallible_person_t(json_value%value_), &
        module_t(MODULE_NAME), &
        procedure_t("from_fallible_json_value"))
  end if
end function

impure elemental function from_errors(errors) result(fallible_person)
  implicit none
  type(error_list_t), intent(in) :: errors
  type(fallible_person_t) :: fallible_person

  fallible_person%errors_ = errors
end function

impure elemental function errors(self) result(errors_)
    implicit none
    class(fallible_person_t), intent(in) :: self
    type(error_list_t) :: errors_

    errors_ = self%errors_
end function

elemental function failed(self) result(failed_)
    implicit none
    class(fallible_person_t), intent(in) :: self
    logical :: failed_

    failed_ = self%errors_%has_any()
end function

function person(self)
  implicit none
  class(fallible_person_t), intent(in) :: self
  type(person_t) :: person

  person = self%person_
end function


end module