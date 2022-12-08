module person_test
  use veggies, only: result_t, test_item_t, describe, it, succeed, fail, assert_that
  use fallible_person_m, only: fallible_person_t
  use erloff, only: error_list_t
  use strff, only: NEWLINE
  use rojff, only: parse_json_from_string, json_null_t


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
    type(error_list_t) :: errors
    character(len=*), parameter :: person = &
       '{                                       ' // NEWLINE &
    // '    "name" : "Giovanni"             ' // NEWLINE &
    // '}                                       '

    maybe_person = fallible_person_t(parse_json_from_string(person))
    if (maybe_person%failed()) then
      errors = maybe_person%errors()
      result_ = fail(errors%to_string())
    else
      result_ = succeed("successfully")
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
    character(len=*), parameter :: person = &
       '{                                       ' // NEWLINE &
    // '    "nme" : "Giovanni"             ' // NEWLINE &
    // '}                                       '

    maybe_person = fallible_person_t(parse_json_from_string(person))
    errors = maybe_person%errors()
    result_ = assert_that(maybe_person%failed(), errors%to_string(), "Should have gotten an error")
  end function
  function check_invalid_name_string() result(result_)
    type(result_t) :: result_
    type(fallible_person_t) :: maybe_person
    type(error_list_t) :: errors
    character(len=*), parameter :: person = &
       '{                                       ' // NEWLINE &
    // '    "name" : 2.0            ' // NEWLINE &
    // '}                                       '

    maybe_person = fallible_person_t(parse_json_from_string(person))
    errors = maybe_person%errors()
    result_ = assert_that(maybe_person%failed(), errors%to_string(), "Should have gotten an error")
  end function
end module
