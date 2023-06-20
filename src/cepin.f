C     Program to read in Cornell Ecology Program (CEP) or Canoco
C     files. The subroutines of this file were originally used in DLL in
C     the vegan library for R, but CRAN no longer accepts Fortran I/O in
C     DLL. This function provides an independent binary that can be
C     called from R and writes the CEP file in a format that can be read
C     into R.
C
C     Copyright (c) 2001--2018 Jari Oksanen
C     Licence: MIT

      program cepreader

      character(len=512) :: cepfile, outfile
      character(len=80) :: fmt
      character(len=8), allocatable:: spnam(:), stnam(:)
      character(len=8) :: arg3
      integer :: kind, nitem, maxdat, nsp, nst
      integer, allocatable :: plotid(:), specid(:), item(:)
      real, allocatable :: abund(:), work(:)

      call get_command_argument(1, cepfile)
      call get_command_argument(2, outfile)
      call get_command_argument(3, arg3)
      read(arg3, '(i8)') maxdat

c     open CEP file
      
      open (unit=1, file=cepfile, status='old')

c     Read 2 or 3 header cards

      call cephead(kind, nitem, nst, fmt)

c     read data

      allocate(plotid(maxdat), specid(maxdat), abund(maxdat))
      allocate(work(nitem), item(nitem))

      select case(kind)
      case (1)
         call cepfree(nitem, maxdat, nsp, nst, plotid, specid,
     x        abund, work, nid)
      case (2)
         call cepopen(fmt, nitem, maxdat, nsp, nst, plotid, specid,
     x        abund, work, nid)
      case (3)
         call cepcond(fmt, nitem, maxdat, nsp, nst, plotid, specid,
     x        abund, work, item, nid)
      case default
         stop 2
      end select

      deallocate(item, work)

c     get names

      allocate(spnam(nsp), stnam(nst))
      call cepnames(spnam, nsp, "spec")
      call cepnames(stnam, nst, "site")

c     Sparse triplet format for R Matrix::sparseMatrix

      open (unit=2, file=outfile, status='new')

      call cep2dgT(plotid, specid, abund, stnam, spnam,
     .     nid, nst, nsp)
      
      close(2)

      deallocate(plotid, specid, abund, stnam, spnam)

      end program cepreader

C*************************************************************
C     Based on my standard cepin module to read in data.
C *************************************************************

C     Read the file header.  Everything needed is the format 'fmt', the
C     'kind' of the file, the number of items in a record ('nitem'), and
C     number of records for 'kind=1'. The `kind' is interpreted from the
C     number of I's in 'fmt'.

      subroutine cephead(kind, nitem, nst, fmt)

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

c     Open CEP format (kind=2).  Zeros are skipped, negative entries
c     stored. Stop when negative site (row) index found.

      subroutine cepopen(fmt, nitem, maxdat, nsp, nst, idplot, idspec, 
     X abund, work, id)

      character (len=80) fmt
      integer nitem, nsp, nst
      integer idplot(maxdat), idspec(maxdat)
      real abund(maxdat)
 
      integer id, ii, j
      real work(nitem)

      nsp = nitem
      nst = 0
      id = 0
      
 20   read (1, fmt) ii, (work(j),j=1,nitem)  
      if (ii .le. 0) return
      if (ii .gt. nst) then
         nst=ii
      end if
      do j=1,nitem
         if (work(j) .ne. 0) then
            id = id+1
            if (id .gt. maxdat) then
               stop 1
            endif
            idplot(id) = ii
            idspec(id) = j
            abund(id) = work(j)
         endif
      end do
      goto 20

      return
      end

c     All entries are stored in condensed format (except zeros)

      subroutine cepcond(fmt, nitem, maxdat, nsp, nst, idplot, idspec, 
     X abund, work, item, id)

      character (len=80) fmt
      integer nitem, nsp, nst
      integer idplot(maxdat), idspec(maxdat)
      real abund(maxdat)
 
      integer id, ii, j
      integer item(nitem)
      real work(nitem)

      nsp = 0
      nst = 0
      id = 0

 40   read (1,fmt) ii,(item(j),work(j),j=1,nitem) 
      if (ii .le. 0) then
         return
      endif
      if (ii .gt. nst) then
         nst = ii
      endif
      do j = 1,nitem
         if (item(j) .gt. 0 .and. work(j) .ne. 0.0) then
            id = id+1
            if (id .gt. maxdat) stop 1
            idplot(id) = ii
            if (item(j) .gt. nsp) then
               nsp = item(j)
            endif
            idspec(id) = item(j)
            abund(id) = work(j)
         endif
      enddo
      goto 40

      return
      end

c     Free CEP format (kind=1) -- or error!  Get everything but
c     zeros.

      subroutine cepfree(nitem, maxdat, nsp, nst, idplot, idspec, 
     X abund, work, id)

      integer nitem, nsp, nst, maxdat
      integer idplot(maxdat), idspec(maxdat)
      real abund(maxdat)
 
      integer id, j, i
      real work(nitem)

      nsp = nitem
      nst = nst
      id = 0

      do i=1,nst
         read(1,*) (work(j),j=1,nsp)
         do j=1,nsp
            if(work(j)  .ne. 0) then
               id=id+1
               if (id .gt. maxdat) then
                  stop 1
               endif
               abund(id) = work(j)
               idspec(id) = j
               idplot(id) = i
            endif
         enddo
      enddo

      return
      end

c     Get row and column names after data cards.

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

c     Names source()d into R cannot have characters that terminate
c     strings ('") or escape terminators (\)

      subroutine sanitname(name)

      character (len=8) :: name
c     baddies are escape sequences for \ ' "
      integer ibad
      character (len=3) ::  baddies
      baddies = '\"'''

      ibad = scan(name, baddies)
      do while(ibad .gt. 0)
         name(ibad:ibad) = ' '
         ibad = scan(name, baddies)
      enddo

      return
      end

c     Write data in sparse matrix triplet form to a temporary file that
c     can be source()d to R and saved there as a sparseMatrix

      subroutine cep2dgT(idplot, idspec, abund, rownames, colnames,
     .     id, nrow, ncol)

      integer :: id, nrow, ncol
      integer :: idplot(id), idspec(id)
      real :: abund(id)
      character(len=8) :: rownames(nrow), colnames(ncol)

c     Sanitize names so that they can source()d into R

      do i=1,ncol
         call sanitname(colnames(i))
      enddo
      do i=1,nrow
         call sanitname(rownames(i))
      enddo

c     Write row and column indices and corresponding abundance data and
c     dimnames in a list that can be used to build a
c     Matrix::sparseMatrix in R

 101  format(99999(i6, :, ","))
 102  format(99999(g13.7, :, ","))
 103  format(99999("'", a8, "'", :, ", "))

      write(2, "('out <- list(')")
      write(2, "('i = c(')")
      write(2, 101) (idplot(i), i = 1,id)
      write(2, "('),')")
      write(2, "('j = c(')")
      write(2, 101) (idspec(i), i = 1,id)
      write(2, "('),')")
      write(2, "('x = c(')")
      write(2, 102) (abund(i), i = 1,id)
      write(2, "('),')")
      write(2, "('inames = c(')")
      write(2, 103) (rownames(i), i=1,nrow)
      write(2, "('),')")
      write(2, "('jnames = c(')")
      write(2, 103) (colnames(i), i=1,ncol)
      write(2, "('))')")

      return
      end
