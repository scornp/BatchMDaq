module VSTypes
implicit none
       integer, parameter :: recordSize = 4
       integer, parameter :: doubleSize = 8
end module VSTypes

module SFTypes
implicit none
       integer, parameter :: recordSize = 4
       integer, parameter :: doubleSize = 2
end module SFTypes

module Utilities
use SFTypes
!use VSTypes
implicit none
contains

  	subroutine byteSwap(realIn, realOutDble )
  	implicit none
  	real, intent(in)   :: realIn
  	real(kind = doubleSize), intent(out)  :: realOutDble
  	integer            :: i_element
  	integer            :: i_element_br
  	real               :: realOut

             i_element = transfer(realIn, 0 )
        call mvbits(i_element, 24, 8, i_element_br, 0  )
        call mvbits(i_element, 16, 8, i_element_br, 8  )
        call mvbits(i_element,  8, 8, i_element_br, 16 )
        call mvbits(i_element,  0, 8, i_element_br, 24 )
               realOut = transfer(i_element_br, 0.0 )
			   realoutDble = realOut
  	return
  	end subroutine byteSwap

  	function getFilterLength(filterFileName)
  	character*(*), intent(in)   :: filterFileName
  	integer                     :: getFilterLength
  	integer                     :: stat, entries

! ok lets open the file
    ! write(6, *) filterFileName
    open(unit=10, file=filterFileName, status="old", iostat=stat)

    if (stat /=0) then
        write(6, *) " File cannot be accessed "
       stop
    end if

! count the number of entries
                entries = 0

    loop : do
                read(10, *, iostat=stat)
            if (stat /=0) then
                exit loop
            else
                entries = entries + 1
             end if
           end do loop

        	if (entries == 0) then
            	! write(6,*) " There are no entries in data.dat...exiting "
            	stop
        	end if
      
           
		   getFilterLength = entries

		close(10)

  	return
  	end function getFilterLength


  	subroutine getFilter(filter, filterLength, filterFileName)
  	character*(*), intent(in)    :: filterFileName
  	integer, intent(in)          :: filterLength
  	real(kind = doubleSize), intent(out)  :: filter(filterLength)
  	integer                      :: stat, i

    open(unit=10, file=filterFileName, status="old", iostat=stat)

  !  load up the arrays
        do i = 1, filterLength
            read(10,*) filter(i)
        end do

	close(10)

   	return
   	end subroutine getFilter

    subroutine getSignalData(dataFile, dataLength, dbleDataSetDble)
    character(len = 1024), intent(in)     :: dataFile
    integer, intent(in)                   :: dataLength
    real(kind = doubleSize), intent(out)  :: dbleDataSetDble(:)
	real, allocatable                     :: dataSet(:)
    integer                               :: i
     
       allocate(dataSet(dataLength)); 
       open(unit = 10, file = trim(adjustl(dataFile)), status = "old", form="unformatted", &
	          & access = "direct", recl = recordSize)
    	 do i = 1, dataLength
    	 !    write(6, *) i
	         read(10, rec = i) dataSet(i)
		 !    call byteSwap(dataSet(i), tmp)
		         dbleDataSetDble(i) = dataSet(i)
	     end do
       close(10)
          write(6, *) "here "
       deallocate(dataSet); 
	return
    end subroutine getSignalData

end module Utilities



!-rnp A simple program to filter data
!-rnp using a band pass filter
program BatchMDaq
use Utilities
implicit none 
logical                       :: debug = .false., test = .false.
integer                       :: filterLength, dataLength
real(kind = doubleSize), allocatable   :: firstDataSetDble(:)
real(kind = doubleSize), allocatable   :: secondDataSetDble(:)
real(kind = doubleSize), allocatable   :: dataArray(:)
real(kind = doubleSize), allocatable   :: filter(:)
real(kind = doubleSize), allocatable   :: signalArray(:)
real(kind = doubleSize)                :: signal  
real(kind = doubleSize)                :: rms
integer                       :: i
character(len = 1024)     :: dataFile1, dataFile2, outputFile, filterName, tagFile
integer                   :: fileSize, m, blockSize
namelist  /inputNML/ dataFile1, dataFile2, outputFile, filterName, tagFile, fileSize


	 write(6, *) 
	 write(6, '(a22)' ) "BatchMDAQ"
	 write(6, '(a22)' ) "---------"
	 write(6, *)

    
    if (debug) write(6, *) "Aquiring namlist "

    read(5, nml=inputNML) 

!	! write(6, nml)


!	set the dataLength as set by MDAQ, could be tidied up!
    
             datalength = fileSize
           filterLength = getFilterLength(trim(adjustl(filterName)))
           
	 write(6, '(a22, a60)' ) "First data file : ",    trim(adjustl(dataFile1))
	 write(6, '(a22, a60)' ) "Second data file : ",   trim(adjustl(dataFile2))
	 write(6, '(a22, a60)' ) "Output file : ",        trim(adjustl(outputFile))
	 write(6, '(a22, a60)' ) "Filter file : ",        trim(adjustl(filterName))
	 write(6, '(a22, a60)' ) "Tag file : ",           trim(adjustl(tagFile))
	 write(6, '(a22, i30)' ) "Data file length : ",   datalength
	 write(6, '(a22, i30)' ) "Filter file length : ", filterLength
        
!	setup the various buffers

     if (debug)  write(6, *) "Allocating memory "
	 allocate(filter(filterLength))
     allocate(firstDataSetDble(dataLength));
     allocate(secondDataSetDble(dataLength));
     allocate(signalArray(dataLength));

! 	now start to get the actual data
     if (debug)  write(6, *) "Aquiring filter "
     call getFilter(filter, filterLength, trim(adjustl(filterName))) 

!	They way the algorithm work is to use the last filterLength - 1 coefficients
!	from the first block in conjucntion with the very first element of the second
!	block to generate the first processed signal elment. But if the fist block does not 
!   exist it is dummied by 0's     

!	The case  of only one data block

     if (debug)  write(6, *) "Aquiring data sets "
	 if (trim(adjustl(dataFile1)) == "") then
	     firstDataSetDble = 0
	 else

!	The case of two data blocks

       call getSignalData(dataFile1, dataLength, firstDataSetDble)
       
	 end if
     
       call getSignalData(dataFile2, dataLength, secondDataSetDble)
       
! 	setup test data
        if (test) then
                firstDataSetDble = 2
                secondDataSetDble = 1
                filter = 1
        end if
! 	end test data
       
      if (debug)   write(6, '(a10, 4f12.6)') "first = ", firstDataSetDble(1), firstDataSetDble(2), &
                   & firstDataSetDble(3), firstDataSetDble(4)       
      if (debug)   write(6, '(a10, 4f12.6)') "second = ", secondDataSetDble(1), secondDataSetDble(2), &
                  & secondDataSetDble(3), secondDataSetDble(4)
       
      ! write(6, *) " Checking data sizes ", size(dataArray(1:filterLength - 1)), &
      !     &   size(firstDataSetDble(dataLength - (filterLength - 1) + 1: dataLength))

      if (debug)  write(6, *) "create the array intto which the unprocessed data will go"  
           
          allocate(dataArray(dataLength + filterLength - 1))

      if (debug)   write(6, *) "Copying data into dataArray " 

!	Get the end of the first data set        
		  dataArray(1:filterLength - 1) &
		  & = firstDataSetDble(dataLength - (filterLength - 1) + 1: dataLength)
           
          dataArray(filterLength:dataLength + filterLength - 1) &
		  & = secondDataSetDble

		  rms = 0.0
          
!	Setup the progress bar

        if (debug)   write(6, *) "Processing data "
        
           blockSize = dataLength/100

         write(6, FMT='(a)', advance="no" )"["
        
		  do m = 1, 100
                write(6, FMT='(a)', advance="no" )"."
          end do
	    
         write(6, FMT='(a)', advance="no" )"]"
         write(6, * )	 

         write(6, FMT='(a)', advance="no" )"["	
      
       do i = 1, dataLength  
             if (mod(i, blockSize) == 0)  write(6, FMT='(a)', advance="no" )"."
                  signal = sum(filter*dataArray(i: i + filterLength - 1))
          signalArray(i) = signal
                     rms = rms + signal*signal
       end do

	     write(6, FMT='(a)', advance="no" )"]" 
         write(6, * )
     
     if (debug)  write(6, *) "Outputing data "   
        
!	 open(unit = 10, file = outputFile, status = "unknown")

     open(unit = 10, file = trim(adjustl(outputFile)), status = "unknown", form="unformatted", &
	          & access = "direct", recl = recordSize)
       do i = 1, dataLength  
           write(10, rec = i) real(signalArray(i)) 
       end do
     close(10)
     	 
	 open(unit = 10, file = tagFile, status = "unknown")
     write(10, *) sqrt(rms/dataLength)
     close(10)

stop
end program BatchMDaq

