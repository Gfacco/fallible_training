module person_test
  use veggies, only: result_t, test_item_t, describe, it, succeed, fail, assert_that, assert_equals
  use fallible_person_m, only: fallible_person_t
  use person_m, only: person_t
  use erloff, only: error_list_t, NOT_FOUND, unknown_type
  use strff, only: NEWLINE
  use rojff, only: parse_json_from_string, json_null_t
  use iso_varying_string, only: var_str, operator(==)


  implicit none
  private
  public :: test_person
contains
  function test_person() result(tests)
    type(test_item_t) :: tests

    tests = describe( &
    "person_test", &
    [ it( &
        "returns a valid person_t from a json object", &
        check_valid_person) &
    , it( &
        "fails if the json input is invalid json", &
        check_invalid_json) &
    , it( &
        "fails if the 'name' key is missing, or misspelled", &
        check_missing_name_key) &
    , it( &
        "fails if the 'name' value is not a string", &
        check_invalid_name_string) &
    ])
  end function

  function check_valid_person() result(result_)
    type(result_t) :: result_
    type(fallible_person_t) :: maybe_person
    type(person_t) :: person
    type(error_list_t) :: errors
    character(len=*), parameter :: person_valid = &
       '{                                       ' // NEWLINE &
    // '    "name" : "Giovanni"             ' // NEWLINE &
    // '}                                       '

    maybe_person = fallible_person_t(parse_json_from_string(person_valid))
    if (maybe_person%failed()) then
      errors = maybe_person%errors()
      result_ = fail(errors%to_string())
    else
      person = maybe_person%person()
      person = maybe_person%person()
      result_ = assert_equals(person%name(), var_str("Giovanni"), "Shold have returned the name 'Giovanni'")
    end if
  end function
  function check_invalid_json() result(result_)
    type(result_t) :: result_
    type(error_list_t) :: errors
    type(fallible_person_t) :: maybe_person

    maybe_person = fallible_person_t(json_null_t())
    errors = maybe_person%errors()
    result_ = assert_that(maybe_person%failed(), errors%to_string(), "Should have gotten an error")

  end function
  function check_missing_name_key() result(result_)
    type(result_t) :: result_
    type(fallible_person_t) :: maybe_person
    type(error_list_t) :: errors
    character(len=*), parameter :: person_no_name_key = &
       '{                                       ' // NEWLINE &
    // '    "nme" : "Giovanni"            ' // NEWLINE &
    // '}                                       '

    maybe_person = fallible_person_t(parse_json_from_string(person_no_name_key))
    errors = maybe_person%errors()
    ! result_ = assert_that(maybe_person%failed(), errors%to_string(), "Should have gotten an error")
    if (maybe_person%failed()) then
      result_ = assert_that((errors.ofType.NOT_FOUND).hasAnyIncluding."name", errors%to_string(), &
        "expected an error of type 'NOT_FOUND' with string 'name.'")
    else
      result_ = fail("Should have gotten and error.")
    end if
  end function
  function check_invalid_name_string() result(result_)
    type(result_t) :: result_
    type(fallible_person_t) :: maybe_person
    type(error_list_t) :: errors
    character(len=*), parameter :: person_name_not_string = &
       '{                                       ' // NEWLINE &
    // '    "name" : 2.0            ' // NEWLINE &
    // '}                                       '

    maybe_person = fallible_person_t(parse_json_from_string(person_name_not_string))
    errors = maybe_person%errors()
    if (maybe_person%failed()) then
    result_ = assert_that((errors.ofType.unknown_type).hasAnyIncluding.'json_string_t', errors%to_string(), &
      "expected an error of type 'unknown_type' with string 'json_string_t.'")
    else 
      result_ = fail("Should have gotten an error.")
    end if
  end function
end module
