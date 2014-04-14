module forsac
! module for reading and writing file with sac format
!
! The sac header is described with the object headersac
! List of routines or functions :
! * sacreadhead(nit,swapon)
!     type(headersac) :: sacreadhead
!     integer :: nit
!     logical :: swapon
! * sacreadtrace(nit,d,n,swapon)
!     integer :: nit, n
!     real, dimension(:) :: d
!     logical :: swapon
! * initsacheader()
!     type(headersac) :: initsacheader
! * minimumheader(n,dt)
!     type(headersac) :: minimumheader
!     integer :: n
!     real :: dt
! * sacwrite(nit,hsac,d,swapon)
!     integer :: nit
!     type(headersac) :: hsac
!     real, dimension(:) :: d
!     logical :: swapon
! * sacopen(filename,nit)
!     integer :: sacopen,nit
!     character(*) :: filename
!######################################################
! Author : André Herrero
! Contact : andherit@gmail.com, andre.herrero@ingv.it
! Public Domain (CC0 1.0 Universal)
!######################################################
! Version 1.0
! to be done :
! * passing nit as optional for sdtin and sdtout
!   swapon may be optional only in the sacwrite routine
! * pass sacwrite and sacread trace to functions for
!   passing error code

implicit none

  type headersac
      real*4 delta,depmin,depmax,scale,odelta,f
      real*4 b,e,o,a,t(10),resp(10),stla
      real*4 stlo,stel,stdp,evla,evlo,evel,evdp
      real*4 mag,user(10),dist,az,baz,gcarc,depmen
      real*4 cmpaz,cmpinc,xminimum,xmaximum,yminimum
      real*4 ymaximum,adjtm
      integer*4 nzyear,nzjday,nzhour,nzmin,nzsec
      integer*4 nzmsec,nvhdr,norid,nevid,npts,nwfid
      integer*4 nxsize,nysize,iftype,idep
      integer*4 iztype,iinst,istreg,ievreg,ievtyp
      integer*4 iqual,isynth,imagtyp,imagsrc
      integer*4 leven,lpspol,lovrok,lcalda
      character*8 kstnm,khole,ko,ka,kt(10),kf
      character*8 kuser(3),kcmpnm,knetwk,kdatrd,kinst
      character*16 kevnm
  end type headersac

  private sac2str,str2sac,swapsac,forceswapsac

contains

!##########################################################################
  function sac2str(sachead)

  type(headersac) :: sachead
  character*632 :: sac2str

  character*4 :: c4
  character*24 :: c24
  character*40 :: c40
  character*80 :: c80

  sac2str(1:4)=transfer(sachead%delta,c4)
  sac2str(5:8)=transfer(sachead%depmin,c4)
  sac2str(9:12)=transfer(sachead%depmax,c4)
  sac2str(13:16)=transfer(sachead%scale,c4)
  sac2str(17:20)=transfer(sachead%odelta,c4)
  sac2str(21:24)=transfer(sachead%b,c4)
  sac2str(25:28)=transfer(sachead%e,c4)
  sac2str(29:32)=transfer(sachead%o,c4)
  sac2str(33:36)=transfer(sachead%a,c4)
  sac2str(41:80)=transfer(sachead%t,c40)
  sac2str(81:84)=transfer(sachead%f,c4)
  sac2str(85:124)=transfer(sachead%resp,c40)
  sac2str(125:128)=transfer(sachead%stla,c4)
  sac2str(129:132)=transfer(sachead%stlo,c4)
  sac2str(133:136)=transfer(sachead%stel,c4)
  sac2str(137:140)=transfer(sachead%stdp,c4)
  sac2str(141:144)=transfer(sachead%evla,c4)
  sac2str(145:148)=transfer(sachead%evlo,c4)
  sac2str(149:152)=transfer(sachead%evel,c4)
  sac2str(153:156)=transfer(sachead%evdp,c4)
  sac2str(157:160)=transfer(sachead%mag,c4)
  sac2str(161:200)=transfer(sachead%user,c40)
  sac2str(201:204)=transfer(sachead%dist,c4)
  sac2str(205:208)=transfer(sachead%az,c4)
  sac2str(209:212)=transfer(sachead%baz,c4)
  sac2str(213:216)=transfer(sachead%gcarc,c4)
  sac2str(225:228)=transfer(sachead%depmen,c4)
  sac2str(229:232)=transfer(sachead%cmpaz,c4)
  sac2str(233:236)=transfer(sachead%cmpinc,c4)
  sac2str(237:240)=transfer(sachead%xminimum,c4)
  sac2str(241:244)=transfer(sachead%xmaximum,c4)
  sac2str(245:248)=transfer(sachead%yminimum,c4)
  sac2str(249:252)=transfer(sachead%ymaximum,c4)
  sac2str(253:256)=transfer(sachead%adjtm,c4)
  sac2str(257:260)=transfer(-12345.,c4)
  sac2str(261:264)=transfer(-12345.,c4)
  sac2str(265:268)=transfer(-12345.,c4)
  sac2str(269:272)=transfer(-12345.,c4)
  sac2str(273:276)=transfer(-12345.,c4)
  sac2str(277:280)=transfer(-12345.,c4)
  sac2str(281:284)=transfer(sachead%nzyear,c4)
  sac2str(285:288)=transfer(sachead%nzjday,c4)
  sac2str(289:292)=transfer(sachead%nzhour,c4)
  sac2str(293:296)=transfer(sachead%nzmin,c4)
  sac2str(297:300)=transfer(sachead%nzsec,c4)
  sac2str(301:304)=transfer(sachead%nzmsec,c4)
  sac2str(305:308)=transfer(sachead%nvhdr,c4)
  sac2str(309:312)=transfer(sachead%norid,c4)
  sac2str(313:316)=transfer(sachead%nevid,c4)
  sac2str(317:320)=transfer(sachead%npts,c4)
  sac2str(325:328)=transfer(sachead%nwfid,c4)
  sac2str(329:332)=transfer(sachead%nxsize,c4)
  sac2str(333:336)=transfer(sachead%nysize,c4)
  sac2str(337:340)=transfer(-12345,c4)
  sac2str(341:344)=transfer(sachead%iftype,c4)
  sac2str(345:348)=transfer(sachead%idep,c4)
  sac2str(349:352)=transfer(sachead%iztype,c4)
  sac2str(353:356)=transfer(-12345,c4)
  sac2str(357:360)=transfer(sachead%iinst,c4)
  sac2str(361:364)=transfer(sachead%istreg,c4)
  sac2str(365:368)=transfer(sachead%ievreg,c4)
  sac2str(369:372)=transfer(sachead%ievtyp,c4)
  sac2str(373:376)=transfer(sachead%iqual,c4)
  sac2str(377:380)=transfer(sachead%isynth,c4)
  sac2str(381:384)=transfer(sachead%imagtyp,c4)
  sac2str(385:388)=transfer(sachead%imagsrc,c4)
  sac2str(389:392)=transfer(-12345,c4)
  sac2str(393:396)=transfer(-12345,c4)
  sac2str(397:400)=transfer(-12345,c4)
  sac2str(401:404)=transfer(-12345,c4)
  sac2str(405:408)=transfer(-12345,c4)
  sac2str(409:412)=transfer(-12345,c4)
  sac2str(413:416)=transfer(-12345,c4)
  sac2str(417:420)=transfer(-12345,c4)
  sac2str(421:424)=transfer(sachead%leven,c4)
  sac2str(425:428)=transfer(sachead%lpspol,c4)
  sac2str(429:432)=transfer(sachead%lovrok,c4)
  sac2str(433:436)=transfer(sachead%lcalda,c4)
  sac2str(437:440)=transfer(-12345,c4)
  sac2str(441:448)=transfer(sachead%kstnm,c4)
  sac2str(449:464)=sachead%kevnm
  sac2str(465:472)=transfer(sachead%khole,c4)
  sac2str(473:480)=transfer(sachead%ko,c4)
  sac2str(481:488)=transfer(sachead%ka,c4)
  sac2str(489:568)=transfer(sachead%kt,c80)
  sac2str(569:576)=transfer(sachead%kf,c4)
  sac2str(577:600)=transfer(sachead%kuser,c24)
  sac2str(601:608)=transfer(sachead%kcmpnm,c4)
  sac2str(609:616)=transfer(sachead%knetwk,c4)
  sac2str(617:624)=transfer(sachead%kdatrd,c4)
  sac2str(625:632)=transfer(sachead%kinst,c4)
  return
end function sac2str

!##########################################################################
  function str2sac(lsachead)

  type(headersac) :: str2sac
  character*632 :: lsachead

  real :: r
  integer :: i
  real, dimension(10) :: r10
  character*8, dimension(10) :: k10
  character*8, dimension(3) :: k3

  str2sac%delta=transfer(lsachead(1:4),r)
  str2sac%depmin=transfer(lsachead(5:8),r)
  str2sac%depmax=transfer(lsachead(9:12),r)
  str2sac%scale=transfer(lsachead(13:16),r)
  str2sac%odelta=transfer(lsachead(17:20),r)
  str2sac%b=transfer(lsachead(21:24),r)
  str2sac%e=transfer(lsachead(25:28),r)
  str2sac%o=transfer(lsachead(29:32),r)
  str2sac%a=transfer(lsachead(33:36),r)
  str2sac%t=transfer(lsachead(41:80),r10)
  str2sac%f=transfer(lsachead(81:84),r)
  str2sac%resp=transfer(lsachead(85:124),r10)
  str2sac%stla=transfer(lsachead(125:128),r)
  str2sac%stlo=transfer(lsachead(129:132),r)
  str2sac%stel=transfer(lsachead(133:136),r)
  str2sac%stdp=transfer(lsachead(137:140),r)
  str2sac%evla=transfer(lsachead(141:144),r)
  str2sac%evlo=transfer(lsachead(145:148),r)
  str2sac%evel=transfer(lsachead(149:152),r)
  str2sac%evdp=transfer(lsachead(153:156),r)
  str2sac%mag=transfer(lsachead(157:160),r)
  str2sac%user=transfer(lsachead(161:200),r10)
  str2sac%dist=transfer(lsachead(201:204),r)
  str2sac%az=transfer(lsachead(205:208),r)
  str2sac%baz=transfer(lsachead(209:212),r)
  str2sac%gcarc=transfer(lsachead(213:216),r)
  str2sac%depmen=transfer(lsachead(225:228),r)
  str2sac%cmpaz=transfer(lsachead(229:232),r)
  str2sac%cmpinc=transfer(lsachead(233:236),r)
  str2sac%xminimum=transfer(lsachead(237:240),r)
  str2sac%xmaximum=transfer(lsachead(241:244),r)
  str2sac%yminimum=transfer(lsachead(245:248),r)
  str2sac%ymaximum=transfer(lsachead(249:252),r)
  str2sac%adjtm=transfer(lsachead(253:256),r)
  str2sac%nzyear=transfer(lsachead(281:284),i)
  str2sac%nzjday=transfer(lsachead(285:288),i)
  str2sac%nzhour=transfer(lsachead(289:292),i)
  str2sac%nzmin=transfer(lsachead(293:296),i)
  str2sac%nzsec=transfer(lsachead(297:300),i)
  str2sac%nzmsec=transfer(lsachead(301:304),i)
  str2sac%nvhdr=transfer(lsachead(305:308),i)
  str2sac%norid=transfer(lsachead(309:312),i)
  str2sac%nevid=transfer(lsachead(313:316),i)
  str2sac%npts=transfer(lsachead(317:320),i)
  str2sac%nwfid=transfer(lsachead(325:328),i)
  str2sac%nxsize=transfer(lsachead(329:332),i)
  str2sac%nysize=transfer(lsachead(333:336),i)
  str2sac%iftype=transfer(lsachead(341:344),i)
  str2sac%idep=transfer(lsachead(345:348),i)
  str2sac%iztype=transfer(lsachead(349:352),i)
  str2sac%iinst=transfer(lsachead(357:360),i)
  str2sac%istreg=transfer(lsachead(361:364),i)
  str2sac%ievreg=transfer(lsachead(365:368),i)
  str2sac%ievtyp=transfer(lsachead(369:372),i)
  str2sac%iqual=transfer(lsachead(373:376),i)
  str2sac%isynth=transfer(lsachead(377:380),i)
  str2sac%imagtyp=transfer(lsachead(381:384),i)
  str2sac%imagsrc=transfer(lsachead(385:388),i)
  str2sac%leven=transfer(lsachead(421:424),i)
  str2sac%lpspol=transfer(lsachead(425:428),i)
  str2sac%lovrok=transfer(lsachead(429:432),i)
  str2sac%lcalda=transfer(lsachead(433:436),i)
  str2sac%kstnm=lsachead(441:448)
  str2sac%kevnm=lsachead(449:464)
  str2sac%khole=lsachead(465:472)
  str2sac%ko=lsachead(473:480)
  str2sac%ka=lsachead(481:488)
  str2sac%kt=transfer(lsachead(489:568),k10)
  str2sac%kf=lsachead(569:576)
  str2sac%kuser=transfer(lsachead(577:600),k3)
  str2sac%kcmpnm=lsachead(601:608)
  str2sac%knetwk=lsachead(609:616)
  str2sac%kdatrd=lsachead(617:624)
  str2sac%kinst=lsachead(625:632)
end function str2sac

!##########################################################################
function sacreadhead(nit,swapon)
! this function reads the header of a trace for a sac
! format. If the unit number nit is positive, it
! considers that a file has been opened in access=direct
! on this unit with a recl=1 (e.g. with function sacopen).
! If nit is negative, it considers that the reading is done
! on the stdin.
! It returns the header in a structure (sachead)
! along with a flag indicating if the binary
! data has to be swapped for the reading (see swapsac
! comment).

  type(headersac) :: sacreadhead
  integer :: nit
  logical :: swapon

  integer :: i
  character*632 :: lsac
 
  if (nit.lt.0) then
     do i=1,632
        call fget(lsac(i:i))
     enddo
  else
     do i=1,632
        read(nit,rec=i) lsac(i:i)
     enddo
  endif 
  swapon=swapsac(lsac,.false.)
  sacreadhead=str2sac(lsac)
return
end function sacreadhead

!##########################################################################
subroutine sacreadtrace(nit,d,n,swapon)

  integer :: nit,n
  logical :: swapon
  real, dimension(:) :: d

  integer :: i,k,idx,ids
  real :: r
  character*4 sa

  do i=1,n
     do k=1,4
        ids=k
        if (swapon) ids=5-k
        if (nit.lt.0) then
           call fget(sa(ids:ids))
        else
           idx=632+(i-1)*4+k
           read(nit,rec=idx) sa(ids:ids)
        endif
     enddo
     d(i)=transfer(sa,r)
  enddo
  return
end subroutine sacreadtrace

!##########################################################################
function swapsac(lsac,notify)
! this function checks the bit order of the sac
! header using the nvhdr parameter. if necessary
! all the header variables are swapped and the
! function returns .true.
! use logical notify to write a warning on sdterr

  logical :: swapsac,notify
  character*632 :: lsac

  integer :: i,j
  character*1 :: mem
  type(headersac) :: sachead

  swapsac=.false.
  sachead=str2sac(lsac)
  if (sachead%nvhdr.lt.0.or.sachead%nvhdr.gt.7) then
     swapsac=.true.
     if (notify) write(0,*) 'sac header need to be swap'
     do i=1,440,4
        do j=1,2
           mem=lsac(i+j-1:i+j-1)
           lsac(i+j-1:i+j-1)=lsac(i-j+4:i-j+4)
           lsac(i-j+4:i-j+4)=mem
        enddo
     enddo
  endif
  return
end function swapsac

!##########################################################################
subroutine forceswapsac(lsac)
! this routine swaps the bit order of the sac
! header. It must be used only on writing
! procedure when the output must be in a non
! native binary.

  character*632 :: lsac

  integer :: i,j
  character*1 :: mem

  do i=1,440,4
     do j=1,2
        mem=lsac(i+j-1:i+j-1)
        lsac(i+j-1:i+j-1)=lsac(i-j+4:i-j+4)
        lsac(i-j+4:i-j+4)=mem
     enddo
  enddo
  return
end subroutine forceswapsac

!##########################################################################
function initsacheader()
!
! this function returns a sac header with its real and
! integer parts initialized to the sac undefined value (-12345). The
! character string are initialized to ''.

  type(headersac) :: initsacheader

  integer :: ia,i,idx
  real :: a
  character*4 :: sa
  character*632 :: lsac

  a=-12345.0
  sa=transfer(a,sa)
  do i=1,70
     idx=(i-1)*4+1
     lsac(idx:idx+3)=sa
  enddo
  ia=-12345
  sa=transfer(ia,sa)
  do i=1,40
     idx=280+(i-1)*4+1
     lsac(idx:idx+3)=sa
  enddo
  do i=441,632
     lsac(i:i)=char(32)
  enddo
  initsacheader=str2sac(lsac)
  return
end function initsacheader


!##########################################################################
function minimumheader(n,dt)
!
! this routine creates a minimal header containing
! the sampling rate dt, the sampling number and six
! other variables to insure the compatibility with
! sac2000.
!
  type(headersac) :: minimumheader
  integer :: n
  real :: dt

  minimumheader=initsacheader()
  minimumheader%npts=n
  minimumheader%delta=dt
  minimumheader%nvhdr=6
  minimumheader%iftype=1
  minimumheader%leven=1
  minimumheader%b=0.
  minimumheader%e=(n-1)*dt
  return
end function minimumheader

!##########################################################################
subroutine sacwrite(nit,hsac,d,swapon)
! this routine write a whole sac trace (header+datastream)
! If the unit number nit is positive, it considers that a
! file has been opened in access=direct on this unit. If
! nit is negative, it considers that the writing is done
! on the stdout.
! It receives the header structure (hsac) and the data stream
! as a real array d.
! a flag indicates if the binary data has to be swapped
! for the writing.

  type(headersac) :: hsac
  integer :: nit
  logical :: swapon
  real, dimension(:) :: d

  integer :: i,k,idx,ids
  character*4 :: sa
  character*632 :: lsac

! computing the extreme values and the mean
  hsac%depmax=maxval(d)
  hsac%depmin=minval(d)
  hsac%depmen=sum(d)/float(hsac%npts)
! swaping the header if necessary
  lsac=sac2str(hsac)
  if (swapon) call forceswapsac(lsac)
! writing the header
  if (nit.lt.0) then
     do i=1,632
        call fput(lsac(i:i))
     enddo
  else
     do i=1,632
        write(nit,rec=i) lsac(i:i)
     enddo
  endif 
! writing the datastream
  do i=1,hsac%npts
     sa=transfer(d(i),sa)
     do k=1,4
        ids=k
        if (swapon) ids=5-k
        if (nit.lt.0) then
           call fput(sa(ids:ids))
        else
           idx=632+(i-1)*4+k
           write(nit,rec=idx) sa(ids:ids)
        endif
     enddo
  enddo
  return
end subroutine sacwrite

!##########################################################################
function sacopen(filename,nit)

  integer :: nit,sacopen
  character(*) :: filename

  open(nit,file=filename(1:len_trim(filename)),form="unformatted",&
access="direct",recl=1,iostat=sacopen)
  return
end function sacopen
     
!##########################################################################
end module forsac
