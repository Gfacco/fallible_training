module person_m
  use iso_varying_string, only: varying_string
implicit none

public :: person_t
type :: person_t
  type(varying_string) :: name_
  type(varying_string) :: location_
contains
  procedure :: name
  procedure :: location
end type

interface person_t
  module procedure construct
end interface

contains
  pure function construct(name, location) result(person)
    type(person_t) :: person
    type(varying_string), intent(in) :: name, location
    person%name_ = name
    person%location_ = location
  end function

  pure function name(self)
    class(person_t), intent(in) :: self
    type(varying_string) :: name
    name = self%name_
  end function

  pure function location(self)
    class(person_t), intent(in) :: self
    type(varying_string) :: location
    location = self%location_
  end function

end module