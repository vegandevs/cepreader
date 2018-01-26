C     main

      program cepreader

      character(len=512) :: cepfile, outfile
      character(len=80) :: line, fmt, fmaxdata
      character(len=8), allocatable:: spnam(:), stnam(:)
      character(len=8) :: arg3
      integer :: kind, nitem, maxdat, id, nsp, nst
      integer, allocatable :: plotid(:), specid(:), item(:)
      real, allocatable :: abund(:), work(:), rdata(:,:)
      
      call getarg(1, cepfile)
      call getarg(2, outfile)
      call getarg(3, arg3)
      read(arg3, '(i8)') maxdat

c     open CEP file
      
      open (unit=1, file=cepfile, status='old')

c     Read 2 or 3 header cards

      call cephead(cepfile, kind, nitem, nst, fmt)

c     read data

      allocate(plotid(maxdat), specid(maxdat), abund(maxdat))
      allocate(work(nitem), item(nitem))

      select case(kind)
      case (1)
      case (2)
         call exit(2)
      case (3)
         call cepcond(fmt, nitem, maxdat, nsp, nst, plotid, specid,
     x        abund, work, item, nid)
      end select

c     get names

      allocate(spnam(nsp), stnam(nst))
      call cepnames(spnam, nsp, "spec")
      call cepnames(stnam, nst, "site")

c     rectangular output file for R

      allocate(rdata(nst, nsp))
      do j=1,nsp
         do i=1,nst
            rdata(i,j) = 0.0
         enddo
      enddo

      do i=1,nid
         rdata(plotid(i), specid(i)) = abund(i)
      enddo
      
c     output
      
      open (unit=2, file=outfile, status='new')

      call cep2rdata(rdata, nst, nsp, stnam, spnam)
      
      close(2)

      end program cepreader
      

C*************************************************************
C Based on my standard cepin module. This had to be vandalized,
C since R cannot interface with character arrays: It can handle only
C a single character*255 variable. So the routine had to be broken
C into pieces, and assume that the file opened  is still there when 
C called repeatedly. 
C
C I feel really bad for this.
C
C Since the subroutine had to be split, I split it completely:
C Each file type is read in a separate subroutine, so that I can
C get rid off static work arrays.
C
C     Copyright (c) Jari Oksanen 2001-2018
C     Licence: MIT (see file in the package)
C *************************************************************

C
C Read the file header.
C The file is left open (that's magic...), and everything
C needed is the format, the `kind' of the file, the number
C of items in a record (`nitem'), and number of records for 
C `kind=1'. The `kind' is interpreted from number of I's in `fmt'. 
C

      subroutine cephead(cepfile, kind, nitem, nst, fmt)

      character (len=255) cepfile
      character(len=80) fmt
      integer kind, nitem, nst

      character (len=80) title
      integer ni, nbrac

      ni=0
      nbrac=0

      read (1,101) title
 101  format (a80)
      read (1,101) fmt
      do i=1,80
         if (fmt(i:i) .eq. 'I' .or. fmt(i:i) .eq. 'i') ni=ni+1
      end do
      kind = ni+1
      if (kind .eq. 1) then
         read(1, *) nitem, nst
         return
      endif
      nbrac=0
      do i=1,80
         if (fmt(i:i) .eq. '(' ) nbrac=nbrac+1
         if (fmt(i:i) .eq. ')' ) then
            nbrac=nbrac-1
            if (nbrac .le. 0) goto 6
         end if
         goto 8
 6       ii=i+2
         if (ii .le. 69) read(fmt,'(t69,i2)') nitem
         if (nitem .eq. 0) then
            read (1,*) nitem
         else
            fmt(ii:80)=' '
         endif
 8       continue
      enddo
      return
      end
      
c
c Open CEP format (kind=2).
c Zeros are skipped, negative entries stored
c Stop when negative site (row) index found.
c
      subroutine cepopen(fmt, nitem, maxdat, nsp, nst, idplot, idspec, 
     X abund, work, ier)

      character (len=255) fmt
      integer nitem, nsp, nst
      integer idplot(maxdat), idspec(maxdat)
      real abund(maxdat)
 
      integer id, ii, j, ier
      real work(nitem)

      nsp = nitem
      nst = 0
      id = 0
      ier = 99
      
 20   read (1, fmt) ii, (work(j),j=1,nitem)  
      if (ii .le. 0) then
         ier = 0
         return
      endif
      if (ii .gt. nst) then
         nst=ii
      end if
      do j=1,nitem
         if (work(j) .ne. 0) then
            id = id+1
            if (id .gt. maxdat) then
               ier = 1
               return
            endif
            idplot(id) = ii
            idspec(id) = j
            abund(id) = work(j)
         endif
      end do
      goto 20
      
      end

C
C Condensed CEP format (kind=3)
C ALL entries are stored in condensed format (except zeros)
C

      subroutine cepcond(fmt, nitem, maxdat, nsp, nst, idplot, idspec, 
     X abund, work, item, id)

      character (len=80) fmt
      integer nitem, nsp, nst
      integer idplot(maxdat), idspec(maxdat)
      real abund(maxdat)
 
      integer id, ii, j, ier
      integer item(nitem)
      real work(nitem)

      nsp = 0
      nst = 0
      id = 0

 40   read (1,fmt) ii,(item(j),work(j),j=1,nitem) 
      if (ii .le. 0) then
         ier = 0
         return
      endif
      if (ii .gt. nst) then
         nst = ii
      endif
      do j = 1,nitem
         if (item(j) .gt. 0 .and. work(j) .ne. 0.0) then
            id = id+1
            if (id .gt. maxdat) call exit(1)
            idplot(id) = ii
            if (item(j) .gt. nsp) then
               nsp = item(j)
            endif
            idspec(id) = item(j)
            abund(id) = work(j)
         endif
      enddo
      goto 40

      end

c      
c Free CEP format (kind=1) -- or error!
c Get everything but zeros.
c

      subroutine cepfree(nitem, maxdat, nsp, nst, idplot, idspec, 
     X abund, work, ier)

      integer nitem, nsp, nst, maxdat
      integer idplot(maxdat), idspec(maxdat)
      double precision abund(maxdat)
 
      integer id, j, i, ier
      double precision work(nitem)

      nsp = nitem
      nst = nst
      id = 0
      ier = 99

      do i=1,nst
         read(1,*) (work(j),j=1,nsp)
         do j=1,nsp
            if(work(j)  .ne. 0) then
               id=id+1
               if (id .gt. maxdat) then
                  ier = 1
                  return
               endif
               abund(id) = work(j)
               idspec(id) = j
               idplot(id) = i
            endif
         enddo
      enddo
      
      ier = 0

      return
      end

c
c     R can handle only a single character*255: 
c     Names cannot be read in one time, but they must be
c     handled line by line.
c

      subroutine cepnames(names, nn, rootn)
      character (len=8) :: names(nn)
      character (len=4) :: rootn
      read (1, '(10a8)', end=666) (names(i), i=1,nn)
      return
c     No names: invent
 666  continue
      do i=1,nn
         write(names(i), '(a4,i4)') rootn,i
      enddo
      return
      end

c     Write opened-up data matrix to a structure that R can read

      subroutine cep2rdata(x, nrow, ncol, rownames, colnames)

      real :: x(nrow, ncol)
      character(len=8) :: rownames(nrow), colnames(ncol)
      integer :: nrow, ncol

c     data.frame

      write(2, "('out <- structure(list(', $)")
      do j= 1,ncol
         write(2, "('c(', $)")
         write(2, "(99999(F5.2, :, ', '), $)") (x(i,j), i = 1,nrow)
         write(2, "(')', $)")
         if (j .lt. ncol) write(2, "(',')")
      enddo
 101  format(99999("'", a8, "'", :, ", "), $)
      write(2, "('), .Names = c(')")
      write(2, 101) (colnames(i), i=1,ncol)
      write(2, "('), row.names = c(')")
      write(2, 101) (rownames(i), i=1,nrow)
      write(2, '("), class = ''data.frame'')")')
      
      return
      end
      
c
c Stupid, but needed
c

      subroutine cepclose()
      close(1)
      return
      end

