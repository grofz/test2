!CONTENT
! module pnmio_module
!   subroutine readpnm(filename,nx,ny,nmax,aa)
!   subroutine writepgm(filename,aa,nmax)
!   subroutine writeppm(filename,rr,gg,bb)
!   subroutine colormap(red,green,blue)
!   subroutine assign_colormap2(aa,rr,gg,bb,n,imin,imax,iwh,ibl)
!   integer function findiu()

! SHORT DESCRIPTION
! These routines work with 'raw' PNM formats

! NOTE 1: these subroutines use system command 'pnmtopnm' for conversions
! between 'raw' and 'plain' formats. If this or similar tool is not
! available then these routines will work with 'plain' formats only.
!
! For writing plain formats -> modify parameter "iplain" in writep?m routines
!
! NOTE 2: It might be necessary replace the IFPORT library (and SYSTEM command) 
! on NON-ifc compilers by a similar command.
! With gfortran compiler, just comment out "use ifport" statement
! 
! WEB: https://netpbm.sourceforge.net/doc/pnm.html

! -----------------------------------------------------------------------------
  module pnmio_module
! -----------------------------------------------------------------------------
! use ifport ! can be commented out with non-ifort compilers (e.g. gfortran)
  implicit none
  integer, external, private  :: findiu

  logical, parameter :: iplain = .false. ! USE this on LINUX, smaller files
  !logical, parameter :: iplain = .true.  ! USE this on WINDOWS

  interface readpnm
    module procedure readpnm
  end interface readpnm

  interface assign_colormap
    module procedure assign_colormap2
    module procedure assign_colormap2R
  end interface assign_colormap

  contains

! -----------------------------------------------------------------------------
  recursive subroutine readpnm(filename,nx,ny,nmax,aa)
! -----------------------------------------------------------------------------
!NOTE: in pnm format specification lines begining with '#' are comments
!      this has not been implemented yet (TODO)
  character(len=*)  :: filename
  integer, optional :: nx, ny
  integer, optional :: nmax
  integer, optional :: aa(:,:)

  ! - local vars
  integer            :: nx1, ny1, nmax1 ! for recursion
  integer            :: fid
  integer            :: mx, my, mmax ! size read from file
  integer            :: ix, iy       ! allocated size of "aa"
  logical            :: ex
  integer            :: magick ! 1,2,3 for PBM,PGM,PPM respectivelly
  character(len=200) :: tmp
  character(len=1)   :: onechar
  integer            :: i, j, ichar

  character(len=300) :: sys1='pnmtopnm -plain'
  character(len=300) :: sys2=' > .tmp.plain' 
  character(len=300) :: sys3='rm -f .tmp.plain' 
  character(len=300) :: sys
  character(len=100) :: tmpfile='.tmp.plain'
  ! to convert 'raw format to plain we use system call:
  ! $ pnmtopnm -plain <filename> > tmpfilename
  ! then we recursivelly call this subroutine again with temporary plain  
  ! file and remove temporary file

  ! - open file & read header to get magick number
  fid = findiu()
  inquire(file=filename, exist=ex)
  if (.not. ex) then
    print *, 'readpnm error: file do not exist '
    print *, trim(filename)
    stop
  endif

  open(fid, file = filename, status = 'old' )

  read(fid,*) tmp ! get the magick number
  select case(trim(tmp))
  case('P1') ! plain PBM format
    magick = 1
    read(fid,*) mx, my
    mmax = 1
  case('P2') ! plain PGM format
    magick = 2
    read(fid,*) mx, my
    read(fid,*) mmax
  case('P3')
    magick = 3
    stop 'readpnm error: P3 (ppm) format not ready, go and code it now!'
  case('P4') ! raw PBM format
    magick = 4
    read(fid,*) mx, my
    mmax = 1
  case('P5') ! raw PGM format
    magick = 5
    read(fid,*) mx, my
    read(fid,*) mmax
  case('P6')
    stop 'readpnm error: P6 (raw ppm) format not ready, go and code it now!'
  case default
    stop 'readpnm error: magick number not recognized, wrong format?'
  end select

  ! - use system call to convert "raw" to "plain" files

! We do not need conversion in order to just get size info
! Uncomment first option if problems with reading dimensions directly from
! RAW files
!
! if (magick > 3) then
  if (magick > 3 .and. present(aa)) then

    print *, 'readpnm info: converting raw format to plain...'
    sys = trim(sys1)//' '//trim(filename)//' '//sys2
    i = SYSTEM(sys)
    if (i /= 0) then
      print *, 'converting failed :',i
      stop
    endif

    if (present(aa)) then
      if (present(nx) .or. present(ny) .or. present(nmax)) then
        print *, 'readpnm warning: "nx", "ny", or "nmax" pars are ignored'
      endif
      call readpnm(filename=tmpfile, aa=aa)
    else ! this branch won't be executed at all
      call readpnm(filename=tmpfile, nx=nx1, ny=ny1, nmax=nmax1)
      if (present(nx))   nx   = nx1
      if (present(ny))   ny   = ny1
      if (present(nmax)) nmax = nmax1
    endif

    print *, 'readpnm info: deleting temporary file...'
    i = SYSTEM(sys3) ! remove temporary file
    if (i /= 0) then
      print *, 'system call (to remove file) failed',i
      stop
    endif

    close(fid)
    return

  endif

  ! - inquire mode only
  if (.not. present(aa)) then
    if (present(nx))   nx   = mx
    if (present(ny))   ny   = my
    if (present(nmax)) nmax = mmax

    close(fid)
    return
  endif

  ! - reading mode ("aa" is present)

  if (present(nx) .or. present(ny) .or. present(nmax)) then
    print *, 'readpnm warning: "nx", "ny", or "nmax" pars are ignored'
  endif

  ix = size(aa,dim=1)
  iy = size(aa,dim=2)

  if (ix /= mx .or. iy /= my) then
    print *, 'readpnm error: allocated size does not match actual file size'
    print *, 'allocated "aa" ', ix, iy
    print *, 'in file        ', mx, my
    stop
  endif

  select case(magick)
  case(1) ! PBM ! pbm is tricky as there are no spaces between ones/zeroes 
    i=0; j=1
    do
      read(fid,'(a)',end=100) tmp

      do ichar=1,len(trim(tmp))
        onechar = tmp(ichar:ichar)
        if (onechar == ' ') cycle
        if (onechar /= '1' .and. onechar /= '0') then
          print *,'readpnm error: invalid character :',onechar
          stop
        endif

        ! now read next byte (update position)
        if (i==ix) then
          i=1
          if (j==iy) then
            print *, 'read_pnm warning: file contains too much data!'
            stop
          endif
          j=j+1
        else
          i=i+1
        endif

        if (onechar=='0') then
          aa(i,j) = 0
        else
          aa(i,j) = 1
        endif

      enddo

      cycle   ! next line from file
      100 exit
    enddo
    
    if (i /= ix .or. j /= iy) then
      print *, 'readpnm error: file ended to soon! '
      print *, 'bytes read ',i,j
      print *, 'bytes expected ',ix,iy
      stop
    endif

print *, 'read_pnm info: PBM file read into aa', float(sum(aa))/count(aa>=0)

  case(2) ! PGM (much easier)
    read(fid,*) aa(1:ix,1:iy)
print *, 'read_pnm info: PGM file read into aa', float(sum(aa))/count(aa>=0)

  case(3) ! PPM (not ready yet)
  case default
print *, magick
    stop 'readpnm unexpected error'
  end select

  close(fid)
! -----------------------------------------------------------------------------
  end subroutine readpnm
! -----------------------------------------------------------------------------



! -----------------------------------------------------------------------------
  subroutine writepgm(filename,aa,nmax)
! -----------------------------------------------------------------------------
  implicit none

  character(len=*)  :: filename
  integer           :: aa(:,:)
  integer, optional :: nmax

  ! - local vars
  !logical, parameter :: iplain = .false. 
  !logical, parameter :: iplain = .true.  ! USE this on WINDOWS

  integer            :: fid   
  integer, parameter :: imax0 = 255
  integer :: ix, iy, imax, i

  character(len=300) :: sys1 = 'pnmtopnm .tmp.plain >'
  character(len=300) :: sys2 = 'rm -f .tmp.plain'
  character(len=300) :: sys

  ! ---

  ix = size(aa,dim=1)
  iy = size(aa,dim=2)
  if (present(nmax)) then
    imax = nmax
    if(imax>999) print *, 'writepgm warning: go to src and change format!'
  else
    imax = imax0
  endif

  fid = findiu()

  if (iplain) then
    open(fid, file = filename, status = 'replace')
  else
    open(fid, file = '.tmp.plain', status = 'replace')
  endif

  write(fid, '(a)') 'P2'
  write(fid, '(i5,1x,i5)') ix, iy
  write(fid, '(i5)') imax
  write(fid, '(10(i3,1x))') aa

  close(fid)

  if (.not. iplain) then ! do system call to convert to raw and delete tempfile

    sys = trim(sys1)//' '//trim(filename)
    print *, 'writepgm info: converting to raw format...'
    i = SYSTEM(sys)
    if (i/=0) then
      print *, 'conversion failed ',i
      stop
    endif

    print *, 'writepgm info: removing temporary file'
    i = SYSTEM(sys2)
    !i = SYSTEM(sys) ! opraveno? TODO
    if (i/=0) then
      print *, 'remove failed ',i
      stop
    endif

  endif
! -----------------------------------------------------------------------------
  end subroutine writepgm
! -----------------------------------------------------------------------------



! -----------------------------------------------------------------------------
  subroutine writeppm(filename,rr,gg,bb)
! -----------------------------------------------------------------------------
  implicit none

  character(len=*)   :: filename
  integer            :: rr(:,:), gg(:,:), bb(:,:)
  integer, parameter :: nmax=255

  ! - local vars
  !logical, parameter :: iplain = .false. 
  !logical, parameter :: iplain = .true.  ! Use this on WINDOWS

  integer            :: fid   
  integer :: ix, iy, ix2, iy2, i, j

  character(len=300) :: sys1 = 'pnmtopnm .tmp.plain >'
  character(len=300) :: sys2 = 'rm -f .tmp.plain'
  character(len=300) :: sys

  ! ---

  ix = size(rr,dim=1)
  iy = size(rr,dim=2)
  ix2 = size(gg,dim=1)
  iy2 = size(gg,dim=2)
  if (ix /= ix2 .or. iy /= iy2) then
    print *, 'writeppm error: red and green arrays not same size'
    stop
  endif
  ix2 = size(bb,dim=1)
  iy2 = size(bb,dim=2)
  if (ix /= ix2 .or. iy /= iy2) then
    print *, 'writeppm error: red and blue arrays not same size'
    stop
  endif

  fid = findiu()

  if (iplain) then
    open(fid, file = filename, status = 'replace')
  else
    open(fid, file = '.tmp.plain', status = 'replace')
  endif

  write(fid, '(a)') 'P3'
  write(fid, '(i5,1x,i5)') ix, iy
  write(fid, '(i5)') nmax
! write(fid, '(10(i3,1x))') aa
  write(fid, '(10(i3,1x))') ((rr(i,j),gg(i,j),bb(i,j), i=1,ix), j=1,iy)

  close(fid)

  if (.not. iplain) then ! do system call to convert to raw and delete tempfile

    sys = trim(sys1)//' '//trim(filename)
    print *, 'writepgm info: converting to raw format...'
    i = SYSTEM(sys)
    if (i/=0) then
      print *, 'conversion failed ',i
      stop
    endif

    print *, 'writepgm info: removing temporary file'
    i = SYSTEM(sys2)
    !i = SYSTEM(sys) ! opraveno? TODO
    if (i/=0) then
      print *, 'remove failed ',i
      stop
    endif

  endif
! -----------------------------------------------------------------------------
  end subroutine writeppm
! -----------------------------------------------------------------------------



! -----------------------------------------------------------------------------
  subroutine colormap(red,green,blue)
! -----------------------------------------------------------------------------
! Generate colormap (blue  cyan  green  yellow  red)
! First and last colors are white and black, respectivelly

  integer, intent(out) :: red(:), green(:), blue(:)

  ! - local vars
  integer, parameter :: mxval = 254
  integer            :: n, i

  real :: x, yr, yb, yg
  ! - 
  n = size(red,dim=1)
  
  do i=2,n-1
    x = float(i-2) / float(n-3) ! x will be in range (0;1)

    yr =  4.*x - 2.
    yb = -4.*x + 2.
    if(x<0.5) then
      yg =  4.*x + 0.
    else
      yg = -4.*x + 4.
    endif
!   yr = -4.*x + 3.
!   yb =  4.*x - 1.
!   if(x<0.5) then
!     yg = -4.*x + 1.
!   else
!     yg =  4.*x - 3.
!   endif

    yr = max(0.,yr)
    yg = max(0.,yg)
    yb = max(0.,yb)
    yr = min(1.,yr)
    yg = min(1.,yg)
    yb = min(1.,yb)

    red(i)   = int(yr*float(mxval+1)) ! values will be between 0..mxval
    green(i) = int(yg*float(mxval+1))
    blue(i)  = int(yb*float(mxval+1))

  enddo

  red(1) = mxval; red(n) = 0
  green(1) = mxval; green(n) = 0
  blue(1) = mxval; blue(n) = 0
! -----------------------------------------------------------------------------
  end subroutine colormap
! -----------------------------------------------------------------------------



! -----------------------------------------------------------------------------
  subroutine assign_colormap2(aa,rr,gg,bb,n,imin,imax,iwh,ibl)
! -----------------------------------------------------------------------------
  integer, intent(in)  :: aa(:,:)
  integer, intent(out) :: rr(:,:), gg(:,:), bb(:,:)
  integer, intent(in)  :: n
  integer, intent(in), optional :: imin, imax, iwh, ibl

  ! - local vars

  integer :: cmap(n,3)
  real    :: dx, fmin, fmax, f
  integer :: i, j, m, nx, ny

  ! -
  call colormap(cmap(1:n,1),cmap(1:n,2),cmap(1:n,3))

  fmin = float(minval(aa))
  fmax = float(maxval(aa))
  if (present(imin)) fmin = float(imin)
  if (present(imax)) fmax = float(imax)
  nx = size(aa,dim=1)
  ny = size(aa,dim=2)
print *, 'assign colormap :',fmin,fmax

  ! to elements aa==fmin assign 2nd color, 
  ! to elements aa==fmax assign (n-1)th color
  ! 1st and nth colors are reserved for elements out of bond, or iwh,ibl 

  dx = (fmax-fmin)/float(n-3)
  
  do i=1,nx
  do j=1,ny
    f = (float(aa(i,j)) - fmin) / dx 
    m = int(f) + 2
!if (m >= n) then
!print *,i,j,aa(i,j),fmax,dx,fmin
!endif
    m = min(max(1,m),n)
    rr(i,j) = cmap(m,1);  gg(i,j) = cmap(m,2);  bb(i,j) = cmap(m,3)
  enddo
  enddo

  if (present(iwh)) then
    where(aa==iwh)
      rr = cmap(1,1) 
      gg = cmap(1,2) 
      bb = cmap(1,3)
    endwhere
  endif
  if (present(ibl)) then
    where(aa==ibl)
      rr = cmap(n,1) 
      gg = cmap(n,2) 
      bb = cmap(n,3)
    endwhere
  endif
! -----------------------------------------------------------------------------
  end subroutine assign_colormap2
! -----------------------------------------------------------------------------



! -----------------------------------------------------------------------------
  subroutine assign_colormap2R(uu,rr,gg,bb,n,rmin,rmax)
! -----------------------------------------------------------------------------
  real(8), intent(in)  :: uu(:,:)
  integer, intent(out) :: rr(:,:), gg(:,:), bb(:,:)
  integer, intent(in)  :: n
  real(8), intent(in), optional :: rmin, rmax

  integer :: aa(size(uu,dim=1), size(uu,dim=2))
  real(8) :: rmin0, rmax0

  ! -
 
  if (present(rmin)) then
    rmin0 = rmin
  else
    rmin0 = minval(uu)
  endif

  if (present(rmax)) then
    rmax0 = rmax
  else
    rmax0 = maxval(uu)
  endif

  aa = nint((uu-rmin0)/(rmax0-rmin0)*float(n))
  call assign_colormap2(aa,rr,gg,bb,n,imin=0,imax=n)
! -----------------------------------------------------------------------------
  end subroutine assign_colormap2R
! -----------------------------------------------------------------------------



! -----------------------------------------------------------------------------
  end module pnmio_module
! -----------------------------------------------------------------------------



! -----------------------------------------------------------------------------
  integer function findiu()
! -----------------------------------------------------------------------------
  logical :: od
  do findiu = 10,1000
    inquire(findiu, opened=od)
    if (.not. od) exit
  enddo
  return
! -----------------------------------------------------------------------------
  end function findiu
! -----------------------------------------------------------------------------

