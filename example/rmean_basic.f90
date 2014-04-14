! Read a sac file and remove the mean.
! Simple exemple to illustrate the use of the module
! forsac
!
! Usage :
!     sprmean < filein.sac > fileout.sac
!
! Dependency
! Module forsac : set of routines to manipulate SAC files.
!        github.com/andherit/forsac
!
!########################################################
! Author : André Herrero
! Contact : andherit@gmail.com, andre.herrero@ingv.it
! Public Domain (CC0 1.0 Universal)
!########################################################
program rmean_basic
use forsac
implicit none

      integer :: i
      integer, parameter :: nit=-1
      real*4, dimension(:), allocatable :: ac
      real*4 :: vmean
      type(headersac) :: lsac
      logical swapon

! Reading the sac file on the sdtin
!  reading the header part
      lsac=sacreadhead(nit,swapon)
!  allocating memory for the signal
      allocate(ac(lsac%npts))
!  reading the time signal
      call sacreadtrace(nit,ac,lsac%npts,swapon)
! Mean removal
      vmean=0.
      do i=1,lsac%npts
         vmean=vmean+ac(i)
      enddo
      vmean=vmean/float(lsac%npts)
      do i=1,lsac%npts
         ac(i)=ac(i)-vmean
      enddo
! Writing the result on the sdtout
      call sacwrite(nit,lsac,ac,swapon)
      stop
end program rmean_basic
