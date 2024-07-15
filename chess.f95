program chess
parameter (m=8)
CHARACTER :: is_continue
logical :: eating_piece, is_eating_piece, is_move, checkmate, checkmate_status
integer :: a(m,m), chess_piece_type, error_code, error_handler, chess_piece_type_on_move
character(len=8), parameter :: rowstext = "12345678"
character :: irowtext

print*, "##################################################################"
print*, "#                                                                #"
print*, "#                WELCOME TO THE CHESS PROGRAM!                   #"
print*, "#			   					  #"
print*, "#                                                     _:_        #" 
print*, "#                                                    '-.-'       #"
print*, "#                                           ()      __.'.__      #"
print*, "#                                        .-:--:-.  |_______|     #"
print*, "#                                 ()      \____/    \=====/      #"
print*, "#                                 /\      {====}     )___(       #"
print*, "#                      (\=,      //\\      )__(     /_____\\     #"
print*, "#      __    |'-'-'|  //  .\    (    )    /____\     |   |       #"
print*, "#     /  \   |_____| (( \_  \    )__(      |  |      |   |       #"
print*, "#     \__/    |===|   ))  `\_)  /____\     |  |      |   |       #"
print*, "#    /____\   |   |  (/     \    |  |      |  |      |   |       #"
print*, "#     |  |    |   |   | _.-'|    |  |      |  |      |   |       #"
print*, "#     |__|    )___(    )___(    /____\    /____\    /_____\\     #"
print*, "#    (====)  (=====)  (=====)  (======)  (======)  (=======)     #"
print*, "#    }===={  }====={  }====={  }======{  }======{  }======={     #"
print*, "#   (______)(_______)(_______)(________)(________)(_________)    #"
print*, "#			   					  #"
print*, "#   PAWN(1)  ROOK(2) KNIGHT(3) BISHOP(4) QUEEN(5)   KING(6)   	  #"
print*, "#			   					  #"
print*, "#           The layout of the chess board is as follows          #" 
print*, "#     please make your selection according to the board below.   #"
print*, "#			   _______________			  #"
print*, "#			1 |_|#|_|#|_|#|_|#|                       #"
print*, "#			2 |#|_|#|_|#|_|#|_|                       #"
print*, "#			3 |_|#|_|#|_|#|_|#|                       #"
print*, "#			4 |#|_|#|_|#|_|#|_|                       #"
print*, "#			5 |_|#|_|#|_|#|_|#|                       #" 
print*, "#			6 |#|_|#|_|#|_|#|_|                       #"
print*, "#			7 |_|#|_|#|_|#|_|#|                       #"
print*, "#			8 |#|_|#|_|#|_|#|_|                       #"
print*, "#			   1 2 3 4 5 6 7 8                        #"
print*, "##################################################################"
print*

120 print*, ("Press c to continue...");
read*, is_continue
if (is_continue /= "c") goto 120

open(11,file="/home/berkeoncel/board.dat")
print*
print*, "Chess Board"
print*
print*, "         ____________________" // &
"___________________________"
do i=1,m
irowtext = trim(rowstext(i:i))
read(11,*) (a(i,j), j=1,m)
write(*,25) irowtext,(a(i,j), j=1,m)
25 format(A8,T10,8(1x,"|",i2," |"))
enddo
print*, "         ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯" // &
"¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯"
print*, "           1     2     3     4     5     6     7     8"

checkmate_status = .false.

do i=1,50

if (checkmate_status .eqv. .true.) exit

icount=mod(i,2)

print*
if(icount.eq.1)  print*, "White's turn"
if(icount.eq.0)  print*, "Black's turn"
print*
198 print*, "*****        which piece do you want to move? (ex:2 1)       *****"
read*, i1,j1
print*
print*, "*****          where do you want to move it? (ex:4 1)        *****"
read*, i2,j2

chess_piece_type = a(i1,j1)
chess_piece_type_on_move = a(i2,j2)
is_move = .true.
error_code = 0
eating_piece = is_eating_piece(icount, chess_piece_type_on_move)

select case(abs(chess_piece_type))

case (0)
error_code = error_handler(is_move,.true.,icount,chess_piece_type)
if (error_code /= 0) goto 198

case (1) !PAWN
call pawn(a,m,i1,i2,j1,j2,icount,eating_piece,chess_piece_type,error_code,is_move)
if (error_code /= 0) goto 198
checkmate_status = checkmate(icount,chess_piece_type_on_move)

case (2) !ROOK 
call rook(a,m,i1,i2,j1,j2,icount,eating_piece,chess_piece_type,error_code,is_move)
if (error_code /= 0) goto 198
checkmate_status = checkmate(icount,chess_piece_type_on_move)

case (3) !KNIGHT
call knight(a,m,i1,i2,j1,j2,icount,eating_piece,chess_piece_type,error_code,is_move)
if (error_code /= 0) goto 198
checkmate_status = checkmate(icount,chess_piece_type_on_move)

case (4) !BISHOP
call bishop(a,m,i1,i2,j1,j2,icount,eating_piece,chess_piece_type,error_code,is_move)
if (error_code /= 0) goto 198
checkmate_status = checkmate(icount,chess_piece_type_on_move)

case (5) !QUEEN
call queen(a,m,i1,i2,j1,j2,icount,eating_piece,chess_piece_type,error_code,is_move)
if (error_code /= 0) goto 198
checkmate_status = checkmate(icount,chess_piece_type_on_move)

case (6) !KING
call king(a,m,i1,i2,j1,j2,icount,eating_piece,chess_piece_type,error_code,is_move)
if (error_code /= 0) goto 198
checkmate_status = checkmate(icount,chess_piece_type_on_move)

end select
enddo
end program chess



subroutine pawn(a,m,i1,i2,j1,j2,icount,eating_piece,chess_piece_type,error_code,is_move)
logical :: eating_piece, first_check, is_move
integer :: a(m,m), chess_piece_type, error_code, error_handler

idiff=i2-i1
jdiff=j2-j1

first_check = .true.

if (chess_piece_type > 0 .and. idiff < 0) then !check pawn back move
is_move = .false.
elseif (chess_piece_type < 0 .and. idiff > 0) then !check pawn back move
is_move = .false.
end if

error_code = error_handler(is_move,first_check,icount,chess_piece_type)
if (error_code == 0) then
	first_check = .false.
	if(a(i2,j2).eq.0.and.abs(idiff).eq.1.and.jdiff.eq.0) then
	a(i1,j1)=0
	a(i2,j2)=chess_piece_type
	elseif(a(i2,j2).eq.0.and.idiff.eq.-2.and.jdiff.eq.0.and.i1.eq.7) then !first move for white pawn
	a(i1,j1)=0
	a(i2,j2)=chess_piece_type
	elseif(a(i2,j2).eq.0.and.idiff.eq.2.and.jdiff.eq.0.and.i1.eq.2) then !first move for black pawn
	a(i1,j1)=0
	a(i2,j2)=chess_piece_type
	elseif(eating_piece.and.abs(idiff).eq.1.and.abs(jdiff).eq.1) then !check eating piece
	a(i1,j1)=0
	a(i2,j2)=chess_piece_type
	else
	error_code = error_handler(is_move,first_check,icount,chess_piece_type)
	endif
endif
call write_file(a,i,j,m,i2,error_code) ! update chess board
end subroutine pawn



subroutine rook(a,m,i1,i2,j1,j2,icount,eating_piece,chess_piece_type,error_code,is_move)
logical :: eating_piece, first_check, is_move
integer :: a(m,m), chess_piece_type, error_code, error_handler

idiff=i2-i1
jdiff=j2-j1

if (jdiff == 0) then
	print*, idiff
        if (idiff < 0) then
		do i = 1, abs(idiff)
		if (is_move .eqv. .false.) exit
		ix = i1 - i
		move_area_piece_type = a(ix,j1)
		if (move_area_piece_type /= 0) is_move = .false. 
		end do
        elseif (idiff > 0) then
		do i = 1, idiff
		if (is_move .eqv. .false.) exit
		ix = i + i1
		move_area_piece_type = a(ix,j1)
		if (move_area_piece_type /= 0) is_move = .false. 
        	end do
        endif
elseif (idiff == 0) then
        if (jdiff < 0) then
		do i = 1, abs(jdiff)
		if (is_move .eqv. .false.) exit
		jx = j1-i
		move_area_piece_type = a(i1,jx)
		if (move_area_piece_type /= 0) is_move = .false. 
        	end do
        elseif (jdiff > 0) then
		do i = 1,jdiff
		if (is_move .eqv. .false.) exit
		jx = j1+i
		move_area_piece_type = a(i1,jx)
		if (move_area_piece_type /= 0) is_move = .false. 
		end do
	endif
endif

first_check = .true.

error_code = error_handler(is_move,first_check,icount,chess_piece_type)
if (error_code == 0) then
	first_check = .false.
	if(a(i2,j2).eq.0.and.abs(idiff).gt.0.and.jdiff.eq.0) then
	a(i1,j1)=0
	a(i2,j2)=chess_piece_type
	elseif(eating_piece.and.abs(idiff).gt.0.and.jdiff.eq.0) then
	a(i1,j1)=0
	a(i2,j2)=chess_piece_type
	elseif(a(i2,j2).eq.0.and.idiff.eq.0.and.abs(jdiff).gt.0) then
	a(i1,j1)=0
	a(i2,j2)=chess_piece_type
	elseif(eating_piece.and.idiff.eq.0.and.abs(jdiff).gt.0) then
	a(i1,j1)=0
	a(i2,j2)=chess_piece_type
	else
	error_code = error_handler(is_move,first_check,icount,chess_piece_type)
	endif
end if
call write_file(a,i,j,m,i2,error_code)
end subroutine rook


subroutine knight(a,m,i1,i2,j1,j2,icount,eating_piece,chess_piece_type,error_code,is_move)
logical :: eating_piece, first_check, is_move
integer :: a(m,m), chess_piece_type, error_code, error_handler

idiff=i2-i1
jdiff=j2-j1

first_check = .true.

if (error_code == 0) then
	first_check = .false.
	if(a(i2,j2).eq.0.and.abs(idiff).eq.2.and.abs(jdiff).eq.1)then
	a(i1,j1)=0
	a(i2,j2)=chess_piece_type
	elseif(abs(a(i1,j1)).eq.3.and.eating_piece.and.abs(idiff).eq.2.and.abs(jdiff).eq.1) then
	a(i1,j1)=0
	a(i2,j2)=chess_piece_type
	elseif(a(i2,j2).eq.0.and.abs(idiff).eq.1.and.abs(jdiff).eq.2)then
	a(i1,j1)=0
	a(i2,j2)=chess_piece_type
	elseif(a(i1,j1).eq.3.and.eating_piece.and.abs(idiff).eq.1.and.abs(jdiff).eq.2) then
	a(i1,j1)=0
	a(i2,j2)=chess_piece_type
	else
	error_code = error_handler(is_move,first_check,icount,chess_piece_type)
	endif
endif
call write_file(a,i,j,m,i2,error_code)
end subroutine knight


subroutine bishop(a,m,i1,i2,j1,j2,icount,eating_piece,chess_piece_type,error_code,is_move)
logical :: eating_piece, first_check, is_move
integer :: a(m,m), chess_piece_type, error_code, error_handler

idiff=i2-i1
jdiff=j2-j1

ix = i1
jy = j1

do i = 1,abs(idiff)
if (is_move .eqv. .false.) exit
if (icount.eq.0) then
	ix = ix - 1 
elseif (icount.eq.1) then
	ix = ix + 1
endif
if (j2 > j1) jy = jy + 1
if (j2 < j1) jy = jy - 1
print*, ix, jy, i
 
move_area_piece_type = a(ix,jy)
if (move_area_piece_type /= 0) is_move = .false. 
end do


first_check = .true.

error_code = error_handler(is_move,first_check,icount,chess_piece_type)
if (error_code == 0) then
	first_check = .false.
	if(a(i2,j2).eq.0.and.abs(idiff).eq.abs(jdiff)) then
	a(i1,j1)=0
	a(i2,j2)=chess_piece_type
	elseif(abs(a(i1,j1)).eq.4.and.eating_piece.and.abs(idiff).eq.abs(jdiff)) then
	a(i1,j1)=0
	a(i2,j2)=chess_piece_type
	else
	error_code = error_handler(is_move,first_check,icount,chess_piece_type)
	endif
end if
call write_file(a,i,j,m,i2,error_code)
end subroutine bishop


subroutine queen(a,m,i1,i2,j1,j2,icount,eating_piece,chess_piece_type,error_code,is_move)
logical :: eating_piece, first_check, is_move
integer :: a(m,m), chess_piece_type, error_code, error_handler

idiff=i2-i1
jdiff=j2-j1


first_check = .true.

if (error_code == 0) then
	first_check = .false.
	if(a(i2,j2).eq.0.and.abs(idiff).gt.0.and.jdiff.eq.0) then
	a(i1,j1)=0
	a(i2,j2)=chess_piece_type
	elseif(abs(a(i1,j1)).eq.5.and.eating_piece.and.abs(idiff).gt.0.and.jdiff.eq.0) then
	a(i1,j1)=0
	a(i2,j2)=chess_piece_type
	elseif(a(i2,j2).eq.0.and.idiff.eq.0.and.abs(jdiff).gt.0) then
	a(i1,j1)=0
	a(i2,j2)=chess_piece_type
	elseif(abs(a(i1,j1)).eq.5.and.eating_piece.and.idiff.eq.0.and.abs(jdiff).gt.0) then
	a(i1,j1)=0
	a(i2,j2)=chess_piece_type
	elseif(a(i2,j2).eq.0.and.abs(idiff).eq.abs(jdiff)) then
	a(i1,j1)=0
	a(i2,j2)=chess_piece_type
	elseif(abs(a(i1,j1)).eq.5.and.eating_piece.and.abs(idiff).eq.abs(jdiff)) then
	a(i1,j1)=0
	a(i2,j2)=chess_piece_type
	else
	error_code = error_handler(is_move,first_check,icount,chess_piece_type)
	endif
endif
call write_file(a,i,j,m,i2,error_code)
end subroutine queen


subroutine king(a,m,i1,i2,j1,j2,icount,eating_piece,chess_piece_type,error_code,is_move)
logical :: eating_piece, first_check, is_move
integer :: a(m,m), chess_piece_type, error_code, error_handler

idiff=i2-i1
jdiff=j2-j1

first_check = .true.

if (error_code == 0) then
	first_check = .false.
	if(a(i2,j2).eq.0.and.abs(idiff).eq.1.and.abs(jdiff).eq.0) then
	a(i1,j1)=0
	a(i2,j2)=chess_piece_type
	elseif(abs(a(i1,j1)).eq.6.and.eating_piece.and.abs(idiff).eq.1.and.abs(jdiff).eq.0) then
	a(i1,j1)=0
	a(i2,j2)=chess_piece_type
	elseif(a(i2,j2).eq.0.and.abs(idiff).eq.0.and.abs(jdiff).eq.1) then
	a(i1,j1)=0
	a(i2,j2)=chess_piece_type
	elseif(abs(a(i1,j1)).eq.6.and.eating_piece.and.abs(idiff).eq.0.and.abs(jdiff).eq.1) then
	a(i1,j1)=0
	a(i2,j2)=chess_piece_type
	elseif(a(i2,j2).eq.0.and.abs(idiff).eq.1.and.abs(jdiff).eq.1) then
	a(i1,j1)=0
	a(i2,j2)=chess_piece_type
	elseif(abs(a(i1,j1)).eq.6.and.eating_piece.and.abs(idiff).eq.1.and.abs(jdiff).eq.1)then
	a(i1,j1)=0
	a(i2,j2)=chess_piece_type
	else
	error_code = error_handler(is_move,first_check,icount,chess_piece_type)
	endif
endif
call write_file(a,i,j,m,i2,error_code)
end subroutine king

! UTILITY FUNCTIONS

subroutine write_file(a,i,j,m,i2,error_code)
integer :: error_code,  a(m,m)
character(len=8), parameter :: rowstext = "12345678"
character :: irowtext

if (error_code == 0) then
print*
print*, "         ____________________" // &
"___________________________"
do i=1,m
irowtext = trim(rowstext(i:i))
write(*,60) irowtext, (a(i,j), j=1,m)
60 format(A8,T10,8(1x,"|",i2," |"))
enddo
print*, "         ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯" // &
"¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯"
print*, "           1     2     3     4     5     6     7     8"
endif
end subroutine write_file


! This function check eating piece
logical function is_eating_piece(icount,chess_piece_type_on_move)
integer, intent(in) :: icount, chess_piece_type_on_move
logical :: eating_piece

if (chess_piece_type_on_move.lt.0.and.icount.eq.1 .or. chess_piece_type_on_move.gt.0.and.icount.eq.0) then 
	is_eating_piece = .true.
else
	is_eating_piece = .false.
end if
end function is_eating_piece


! This function check checkmate status
logical function checkmate(icount,chess_piece_type_on_move)
integer, intent(in) :: icount, chess_piece_type_on_move

if (chess_piece_type_on_move.eq.-6.and.icount.eq.1 .or. chess_piece_type_on_move.eq.6.and.icount.eq.0) then
	print*
	if (icount.eq.1) print*, "*****              White made black checkmate!!              *****"
	if (icount.eq.0) print*, "*****              Black made white checkmate!!              *****"
	print*, "*****                      GAME OVER!!                       *****"
	checkmate = .true.
else
	checkmate = .false.
end if
end function checkmate


! This function find error
integer function error_handler(is_move,first_check,icount,chess_piece_type) 
implicit none
logical, intent(in) :: first_check, is_move
integer, intent(in) :: icount, chess_piece_type

if (icount.eq.1.and.chess_piece_type < 0 .or. icount.eq.0.and.chess_piece_type > 0) then
	error_handler = 1
	
elseif (chess_piece_type == 0) then
	error_handler = 2
	
elseif (first_check .neqv. .true.) then 
	error_handler = 3
	
elseif (is_move .eqv. .false.) then
	
	if (abs(chess_piece_type) == 1) then  
	error_handler = 5
	
	else
	error_handler = 4
	end if
else
	error_handler = 0
endif

select case (error_handler)
case (1)
print*
print*, "*****              this piece not your piece                 *****"
print*, "*****                   plase try again                      *****"
print*
case (2)
print*
print*, "*****                This board area empty!!                *****" 
print*, "*****             Plase choose a filled area!!              *****"
print*
case (3)
print*
print*, "*****              you made an incorrect move!!             *****"
print*, "*****                   plase try again                     *****"
print*
case (4)
print*
print*, "***** There is another piece in the direction of movement!! *****"
print*, "*****                      plase try again                  *****"
print*
case (5)
print*
print*, "*****              Pawn cannot move backwards!!             *****"
print*, "*****                    plase try again                    *****"
print*
end select
end function error_handler

