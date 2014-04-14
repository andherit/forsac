! read a sac file and remove the mean. This version
! is able to do a simple preevent mean removal if the option
! "onlyon" is used on the command line. It indicates 
! the number of seconds on which the mean is computed 
! at the beginning of the signal. If "onlyon" is negative
! or absent, the mean is computed on all the length of
! the signal
!
! Usage :
!     sprmean [onlyon=real] < filein.sac > fileout.sac
!
! Dependency
! Module parser : tools to parse input resquest by keywords
!        github.com/andherit/parser
! Module forsac : set of routines to manipulate SAC files.
!        github.com/andherit/modules
!
!########################################################
! Author : André Herrero
! Contact : andherit@gmail.com, andre.herrero@ingv.it
! Public Domain (CC0 1.0 Universal)
!########################################################
program rmean
use parser
use forsac
implicit none

      integer :: i,np
      integer, parameter :: nit=-1
      real*4, dimension(:), allocatable :: ac
      real*4 :: vmean,onlyon
      type(headersac) :: lsac
      logical swapon

      if (parse_arg("onlyon",onlyon) /= PARSE_OK) onlyon=-1.
      lsac=sacreadhead(nit,swapon)
      allocate(ac(lsac%npts))
      call sacreadtrace(nit,ac,lsac%npts,swapon)
      vmean=0.
      if (onlyon < 0.) then
         np=lsac%npts
      else
         np=min(int(onlyon/lsac%delta),lsac%npts)
      endif
      do i=1,np
         vmean=vmean+ac(i)
      enddo
      vmean=vmean/float(np)
      do i=1,lsac%npts
         ac(i)=ac(i)-vmean
      enddo
      call sacwrite(nit,lsac,ac,swapon)
      stop
end program rmean
