function factorial(parm) result (res)
   implicit none
   integer res, parm, tmp, i

   tmp = 1
   do i = 2, parm
      tmp = tmp * i
   end do

   res = tmp
end function

