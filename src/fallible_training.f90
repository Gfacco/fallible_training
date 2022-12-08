module fallible_training
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    use fallible_person_m, only: fallible_person_t
    use person_m, only: person_t
    use rojff, only: parse_json_from_file
    use erloff, only: error_list_t
    use iso_varying_string, only: operator(//), put_line, char
    use, intrinsic :: iso_fortran_env, only: error_unit, int64



    type(fallible_person_t) :: maybe_person
    type(person_t) :: person
    type(error_list_t) :: errors

    maybe_person = fallible_person_t(parse_json_from_file("person.json"))
    if (maybe_person%failed()) then
      errors = maybe_person%errors()
      call put_line(error_unit, errors%to_string())
    else
      person = maybe_person%person()
      call put_line("Hello, " // person%name())
    end if
  end subroutine say_hello
end module fallible_training
