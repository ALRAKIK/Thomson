subroutine read_f(arg,D,N,tol,iter,Lx,Ly,Lz,ra,mu,sh,he,de,an,or,nlines)

  implicit none 
  
  ! ---- ! input  ! ---- ! 
  
  character (len = 20 ) , intent(in)         :: arg 
  
  ! ---- ! local  ! ---- !  
  
  integer                                    :: i 
  character (len = 10 )                      :: boxa   , boxb 
  character (len = 10 )                      :: ranaa  , ranab
  character (len = 10 )                      :: multib , multia
  character (len = 10 )                      :: showb  , showa
  character (len = 10 )                      :: denb   , dena
  character (len = 10 )                      :: dn     , rect
  character (len = 10 )                      :: hesb   , hesa
  character (len = 10 )                      :: desb   , desa
  character (len = 10 )                      :: animb  , anima
  double precision , parameter               :: pi = acos(-1.0d0)

  
  ! ---- ! output ! ---- !  
  
  integer              , intent(out)         :: D , N , iter , nlines
  double precision     , intent(out)         :: tol 
  double precision     , intent(out)         :: Lx , Ly , Lz
  character (len = 20 ), intent(out)         :: ra , mu ,sh, he , de, an
  character (len = 20 ), intent(out)         :: or
  
  ! ---- ! code  ! ---- !
  
  ! ---- ! initalize ! ---- ! 
  
  nlines = 0
  
  boxb   = "box:"
  ranab  = "random"
  multib = "multiply"
  showb  = "show"
  denb   = "density"
  hesb   = "hessian"
  desb   = "distance"
  animb  = "animation"
  
  !---------------------------------!
  
  open (1, file = arg)
  do
    read (1,*, end=1)
    nlines = nlines + 1
  end do 
  1 close (1)
  
  open (1, file = arg)
    read(1,*)  D 
    read(1,*)  N 
    read(1,*)  tol 
    read(1,*)  iter 
  close (1)
  
  open (1,file = arg)
    do i=1,nlines
    read (1,*,end=2) boxa
    if (boxa .EQ. boxb) then
      backspace (1)
      read  (1,*) boxa , Lx , Ly ,Lz
      close(1)
    end if 
  end do
  2 close(1)


  
  open (1,file = arg)
    do i=1,nlines
    read (1,*,end = 3) ranaa
      if (ranaa .EQ. ranab) then
      backspace (1)
      read  (1,*) ra
      close(1)
      end if 
    end do 
  3 close(1)
  
  open (1,file = arg)
    do i=1,nlines
    read (1,*,end=4) multia
      if (multia .EQ. multib) then
        backspace (1)
        read  (1,*) mu
        close(1)
      end if 
    end do 
  4 close(1)
  
  open (1,file = arg)
    do i=1,nlines
    read (1,*,end=5) showa
      if (showa .EQ. showb) then
        backspace (1)
        read  (1,*) sh
        close(1)
      end if 
    end do 
  5 close(1)
  
   open (1,file = arg)
    do i=1,nlines
    read (1,*,end=6) dena
      if (dena .EQ. denb) then
        backspace (1)
        read  (1,*) dn , rect
        close(1)
      end if 
    end do 
  6 close(1)
  
  if (dn == "density" .and. boxa /= "box") then 
  
    select case (D) 
    
      case (1)
      
      Lx = 2*N
      
      case (2)
      
        if (rect == "rectangle") then 
        
        Lx = sqrt((2*N)/(0.5642*sqrt(3.d0)))
        Ly = sqrt(3.d0)*Lx / 2 
        
        else 
        
        Lx = sqrt(N/0.5642)
        Ly = Lx
        
        end if 
      
      case (3)
        
        Lx = ( N / 0.6204 )**(1.d0/3.d0)
        Ly = Lx
        Lz = Lx 
        
    end select
    
  end if 
  
   open (1,file = arg)
    do i=1,nlines
    read (1,*,end=7) hesa
      if (hesa .EQ. hesb) then
        backspace (1)
        read  (1,*) he
        close(1)
      end if 
    end do 
  7 close(1)
  
    open (1,file = arg)
    do i=1,nlines
    read (1,*,end=8) desa
      if (desa .EQ. desb) then
        backspace (1)
        read  (1,*) de
        close(1)
      end if 
    end do 
  8 close(1)
  
    open (1,file = arg)
    do i=1,nlines
    read (1,*,end=9) anima
      if (anima .EQ. animb) then
        backspace (1)
        read  (1,*) an , or
        close(1)
      end if 
    end do 
  9 close(1)
  
  
  
end subroutine

subroutine title(N,space,iter,geo_typ,Lx,Ly,Lz,file_index)

  implicit none 
  
  ! ---- ! input  ! ---- ! 
  
  integer              , intent(in)          :: N , space , iter
  integer              , intent(in)          :: file_index
  double precision     , intent(in)          :: Lx , Ly , Lz 
  character (len = 10 ), intent(in)          :: geo_typ
  
  ! ---- ! local  ! ---- !  
  
  double precision , parameter               :: pi = acos(-1.0d0)
  double precision                           :: LLx , LLy , LLz 
  
  ! ---- ! output ! ---- !  
  
  
  
  
  ! ---- ! code  ! ---- !

  LLx = Lx/(2*pi)
  LLy = Ly/(2*pi)
  LLz = Lz/(2*pi)
  
write(file_index,'(a)') ""
write(file_index,'(a)') "   _____ _                                                       _     _ "
write(file_index,'(a)') "  |_   _| |                                                     | |   | |"
write(file_index,'(a)') "    | | | |__   ___  _ __ ___  ___  ___  _ __    _ __  _ __ ___ | |__ | | ___ _ __ ___"
write(file_index,'(a)') "    | | | '_ \ / _ \| '_ ` _ \/ __|/ _ \| '_ \  | '_ \| '__/ _ \| '_ \| |/ _ \ '_ ` _ \"
write(file_index,'(a)') "    | | | | | | (_) | | | | | \__ \ (_) | | | | | |_) | | | (_) | |_) | |  __/ | | | | |"
write(file_index,'(a)') "    \_/ |_| |_|\___/|_| |_| |_|___/\___/|_| |_| | .__/|_|  \___/|_.__/|_|\___|_| |_| |_|"
write(file_index,'(a)') "                                                | |"
write(file_index,'(a)') "                                                |_|"
write(file_index,'(a)') ""
write(file_index,'(a)') "                           ,,....................,,.                 "
write(file_index,'(a)') "                     ,..........,,,,..      ............,*           "
write(file_index,'(a)') "                 ,.......,,,,,****************,,,,,,.......,,*       "
write(file_index,'(a)') "              *,.....,,,,***/////((((((((((/////****,,,......,,*     "
write(file_index,'(a)') "            /,,.....,,**//(((###%%%%%%%%%%%%###(((//**,,,.....,,*/   "
write(file_index,'(a)') "           /*,,.....,,*/(##%%%%%%%%%%%%%%%%%%%%%%%##(/**,......,,*/  "
write(file_index,'(a)') "          (/*,,......,*/#%%%,                     %%%#/,,.....,,**/( "
write(file_index,'(a)') "          #/**,,,......,,                            ,......,,,**/(# "
write(file_index,'(a)') "          (#(/**,,,,........,                    ,.......,,,***/((#% "
write(file_index,'(a)') "           %#((//***,,,,,..........................,,,,,,***//((#%%  "
write(file_index,'(a)') "            *%%#((///*****,,,,,.       ....,,,,,,,*****///(((#%%%%   "
write(file_index,'(a)') "              #%%%##((((/////******************/////((((###%%%%%     "
write(file_index,'(a)') "                 %%%%%%%####((((((((((((((((((((####%%%%%%%%%        "
write(file_index,'(a)') "                     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%            "
write(file_index,'(a)') "                            #%%%%%%%%%%%%%%%%%%%%%                   "
write(file_index,'(a)') ""
write(file_index,'(a)') " By : A.ALRAKIK                                                Supervision: S.Evangelisti   "
write(file_index,'(a)') "____________________________________________________________________________________________"
write(file_index,'(a)') ""

if (  geo_typ == "random" ) then

  write(file_index,'(a,i4,a,i1,a,i7,a,a)') "The calculation for " , N ,"  electron in " , space   &
                                 ,"D dimension , with ", iter , " loops and random geometry"

else

  write(file_index,'(a,i4,a,i1,a,i7,a,a)') "The calculation for " , N ,"  electron in " , space   &
                                 ,"D dimension , with ", iter , " loops and spacific geometry"

end if

write(file_index,'(a)') "____________________________________________________________________________________________"
write(file_index,'(a)') ""
write(file_index,'(a)') "                   || The Pi parameter ||"
write(file_index,'(a)') ""
write(file_index,'(a,f14.12,a,f14.12)') "      Pi = " , pi , "       2 Pi = " , 2.d0*pi
write(file_index,'(a)') ""
write(file_index,'(a)') "                   ||  The size of the box  ||"
write(file_index,'(a)') ""
write(file_index,'(a,f14.12,a,f14.12,a,f14.12,a)') "       X = " , Lx ,"          Y = " , Ly , "          Z = " , Lz
write(file_index,'(a)') ""
write(file_index,'(a)') "                   ||  The ratio between 2 pi and the lengths of the box  ||"
write(file_index,'(a)') ""
write(file_index,'(a,f14.12,a,f14.12,a,f14.12)') "ratio(x) = " , LLx ,"   ratio(y) = " , LLy , "   ratio(z) = " , LLz
write(file_index,'(a)') ""
write(file_index,'(a)') "____________________________________________________________________________________________"
write(file_index,'(a)') "                               _                _             _ "
write(file_index,'(a)') "                          ___ | |_  __ _  _ __ | |_  ___   __| |"
write(file_index,'(a)') "                         / __|| __|/ _` || '__|| __|/ _ \ / _` |"
write(file_index,'(a)') "                         \__ \| |_| (_| || |   | |_|  __/| (_| |"
write(file_index,'(a)') "                         |___/ \__|\__,_||_|    \__|\___| \__,_|"
write(file_index,'(a)') "____________________________________________________________________________________________"
write(file_index,'(a)') ""

end subroutine

subroutine first_geo(arg,N,D,Lx,Ly,Lz,ra,mu,geo,nlines)
      
  implicit none 

  ! ---- ! input  ! ---- ! 
  
  integer              , intent(in)          :: N  , D 
  double precision     , intent(in)          :: Lx , Ly , Lz 
  character (len = 20 ), intent(in)          :: arg , mu , ra
  integer              , intent(in)          :: nlines 
  
  ! ---- ! local  ! ---- !  

  integer                                    :: i , k 
  integer                                    :: null 
  character (len = 10 )                      :: geob , geoa
  
  
  ! ---- ! output ! ---- !  
  
  double precision      , intent(out)        :: geo(N,D)
  
  ! ---- ! code  ! ---- !
    
    
      
  if (ra == "random") then 
    call random_number(geo)
  else
  
  geob   = "geometry"  
  
  open (5,file = arg)
        
  do i=1,nlines
    read (5,*,end=2) geoa
      if (geoa .EQ. geob) then
        backspace (5)
        read  (5,*) geoa
          do k = 1,N
            if      (D == 3) then 
                read  (5,*)  null , geo(k,1) , geo(k,2) , geo(k,3)   
            else if (D == 2) then 
                read  (5,*)  null , geo(k,1) , geo(k,2)
            else if (D == 1) then
                read  (5,*)  null , geo(k,1) 
            end if 
          end do 
          close(5)
      end if 
  end do
    
  2 close (1)
  
  end if 
  
  
  if (mu == "multiply") then 
      
      if      (D == 3) then 
      
      geo(:,1) = geo(:,1)* Lx 
      geo(:,2) = geo(:,2)* Ly
      geo(:,3) = geo(:,3)* Lz
      
      else if (D == 2) then 
      
      geo(:,1) = geo(:,1)* Lx 
      geo(:,2) = geo(:,2)* Ly
      
      else if (D == 1) then
      
      geo(:,1) = geo(:,1)* Lx
      
      end if 
      
  end if 


end subroutine

subroutine print_geo(N,D,geo,file_index)

  implicit none 
	
  ! ---- ! input  ! ---- ! 
  
  integer              , intent(in)          :: N  , D  
  integer              , intent(in)          :: file_index
  double precision     , intent(in)          :: geo(N,D)
  
  ! ---- ! local  ! ---- !  
  
  integer                                    :: i 
  
  ! ---- ! output ! ---- !  
  
  
  
  ! ---- ! code  ! ---- !
	
  write(file_index,'(a)') ""
  if      ( D == 3 ) then
  
    
    write (file_index,'(a)') "                 =========================================================================="
    write (file_index,'(a)') "                           X                         Y                         Z"
    write (file_index,'(a)') "                 =========================================================================="
    write (file_index,'(a)') ""
    
    do i = 1, N
      write (file_index,'(I4,a,f26.16,f26.16,f26.16)') i,"      " , geo(i, :)
    end do 
  
  else if ( D == 2 ) then 
    
    
    write (file_index,'(a)') "                 ==============================================="
    write (file_index,'(a)') "                           X                         Y"
    write (file_index,'(a)') "                 ==============================================="
    write (file_index,'(a)') ""
    
    do i = 1, N
      write (file_index,'(I4,a,f26.16,f26.16)') i,"      " , geo(i, :)
    end do 
	
  else if ( D == 1 ) then 
    
    
    write (file_index,'(a)') "                 ===================="
    write (file_index,'(a)') "                           X"
    write (file_index,'(a)') "                 ===================="
    write (file_index,'(a)') ""
    
    do i = 1, N
      write (file_index,'(I4,a,f26.16)') i,"      " , geo(i, :)
    end do 
  
  end if 
    
    write(file_index,'(a)') ""
  
  end subroutine
  
subroutine PBC(N,D,X,Lx,Ly,Lz)

  implicit none 
	
  ! ---- ! input  ! ---- ! 
  
  integer              , intent(in)          :: N  , D  
  double precision     , intent(in)          :: Lx,Ly,lz
  
  ! ---- ! local  ! ---- !  
  
  integer                                    :: i 
  
  ! ---- ! output ! ---- !  
  
  double precision  , intent(inout)          :: X(N,D)
  
  ! ---- ! code  ! ---- !
	
  if ( D == 3 ) then 
    
    do i = 1,N
    
      do while (X(i,1) <= 0 )
         X(i,1) = X(i,1) + Lx
      end do 
      
      do while (X(i,2) <= 0 )
         X(i,2) = X(i,2) + Ly
      end do
      
      do while (X(i,3) <= 0 )
         X(i,3) = X(i,3) + Lz
      end do
      
      do while (X(i,1) > Lx)
        X(i,1) = X(i,1) - Lx 
      end do 
      
      do while (X(i,2) > Ly)
        X(i,2) = X(i,2) - Ly 
      end do 
      
      do while (X(i,3) > Lz)
        X(i,3) = X(i,3) - Lz 
      end do 
      
    end do
    
  end if 
	
  if ( D == 2 ) then 
    
    do i = 1,N

      do while (X(i,1) <= 0 )
         X(i,1) = X(i,1) + Lx
      end do 
      
      do while (X(i,2) <= 0 )
         X(i,2) = X(i,2) + Ly
      end do
      
      do while (X(i,1) > Lx)
        X(i,1) = X(i,1) - Lx 
      end do 
      
      do while (X(i,2) > Ly)
        X(i,2) = X(i,2) - Ly 
      end do 
       
    end do
    
  end if 
  
    if ( D == 1 ) then 
    
    do i = 1,N
    
      do while (X(i,1) <= 0 )
         X(i,1) = X(i,1) + Lx
      end do 
      
      do while (X(i,1) > Lx)
        X(i,1) = X(i,1) - Lx 
      end do 
     
    end do
    
  end if 
	  
  end subroutine
  
  subroutine N_geo(N,D,X,lambda,conj_s)
  
  implicit none 
	
  ! ---- ! input  ! ---- ! 
  
  integer              , intent(in)          :: N  , D  
  double precision     , intent(in)          :: lambda(1,1)
  double precision     , intent(in)          :: conj_s(D*N,1)
  
  ! ---- ! local  ! ---- !  
  
  integer                                    :: i 
  
  ! ---- ! output ! ---- !  
  
  double precision  , intent(inout)          :: X(N,D)
  
  ! ---- ! code  ! ---- !
  
    if (D == 3) then 
      do i=1,N
        X(i,1) = X(i,1)+lambda(1,1)*conj_s(i,1)
        X(i,2) = X(i,2)+lambda(1,1)*conj_s(i+N,1)
        X(i,3) = X(i,3)+lambda(1,1)*conj_s(i+2*N,1)
      end do
    end if 
    
    if (D == 2) then 
      do i=1,N
        X(i,1) = X(i,1)+lambda(1,1)*conj_s(i,1)
        X(i,2) = X(i,2)+lambda(1,1)*conj_s(i+N,1)
      end do
    end if
    
    if (D == 1) then 
      do i=1,N
        X(i,1) = X(i,1)+lambda(1,1)*conj_s(i,1)
      end do
    end if
  
  end subroutine
  
  subroutine diagonalize_matrix(N,A,e,file_index)

! Diagonalize a square matrix

  implicit none

! Input variables

  integer,intent(in)            :: N
  integer,intent(in)            :: file_index
  double precision,intent(inout):: A(N,N)
  double precision,intent(out)  :: e(N)

! Local variables

  integer                       :: lwork,info
  integer                       :: i
  double precision,allocatable  :: work(:)
  

! Memory allocation

  allocate(work(3*N))
  lwork = size(work)

  call dsyev('V','U',N,A,N,e,work,lwork,info)
 
  if(info /= 0) then 
    write(file_index,'(a)') 'Problem in diagonalize_matrix (dsyev)!!'
    stop
  endif
  
  do i = 1 , N
    if (abs(e(i)) < 1e-10) e(i) = 0
  end do  
  
  write(file_index,'(a)') "____________________________________________________________________________________________"
  write(file_index,'(a)') ""
  write(file_index,'(a)') "            The EigenValues of the hessian"
  write(file_index,'(a)')
  
  do i = 1,N
    write (file_index,'(f20.12)') e(i)  
  end do
  
  write(file_index,'(a)') "____________________________________________________________________________________________"
  write(file_index,'(a)') ""
  
    do i=1,N
      if (abs(e(i)) > 1e-8 .and. e(i) < 0) then 
        write (file_index,'(a)') "you are at stationary state (Negative eignvalue)"
        write (file_index,'(a)') ""
        exit
      else 
        write (file_index,'(a)') "all the eignvalue is positive"
        write (file_index,'(a)') ""
        exit
      end if 
    end do 
  

  end subroutine diagonalize_matrix
  
  subroutine print_distance(N,D,X,Lx,Ly,Lz,file_index)
  
  implicit none 
  
  ! Input variables

  integer,intent(in)            :: N , D 
  integer,intent(in)            :: file_index
  double precision ,intent(in)  :: X(N*D,N*D) 
  double precision ,intent(in)  :: Lx,Ly,Lz

! Local variables

  integer                       :: i , j 
  double precision , parameter  :: pi = acos(-1.0d0)
  double precision              :: LLx,LLy,LLz
  double precision              :: X_distance(N,N)
  
	
  LLx = Lx/(2*pi)
  LLy = Ly/(2*pi)
  LLz = Lz/(2*pi) 
   
  if (D == 1) then
    
  do i = 1,N
        do j = 1 , N
                X_distance(i,j) = sqrt((X(i,1)-X(j,1))**2)
        end do 
  end do
    
  else if (D == 2 ) then 
    
    do i = 1,N
        do j = 1 , N
                X_distance(i,j) = sqrt((X(i,1)-X(j,1))**2+(X(i,2)-X(j,2))**2)
        end do 
    end do
    
  else if (D == 3) then 
  
    do i = 1,N
        do j = 1 , N
                X_distance(i,j) = sqrt((X(i,1)-X(j,1))**2 + (X(i,2)-X(j,2))**2 + (X(i,3)-X(j,3))**2)
        end do 
    end do
    
  end if 
    
    write(file_index,'(a)') "____________________________________________________________________________________________"
    write(file_index,'(a)') ""
    write(file_index,'(a)') '                                    The distance matrix (Geodesic)'
    write(file_index,'(a)') ""
    write(file_index,'(a)') ""
     
    call matout(N,N,X_distance,file_index)
	
    X_distance = 0.d0

    
    if (D == 1) then 
    
    do i = 1,N
        do j = 1 , N
            X_distance(i,j) = (LLx**(-2)*(2.d0-2.d0*cos(LLx*(X(i,1)-X(j,1)))))**(0.5d0)
        end do 
    end do
    
    
    else if (D == 2) then 
    
    do i = 1,N
        do j = 1 , N
            X_distance(i,j) = (LLx**(-2)*(2.d0-2.d0*cos(LLx*(X(i,1)-X(j,1))))&
                              +LLy**(-2)*(2.d0-2.d0*cos(LLy*(X(i,2)-X(j,2)))))**(0.5d0)
        end do 
    end do
    
    
    else if (D == 3) then 
    
    do i = 1,N
        do j = 1 , N
            X_distance(i,j) = (LLx**(-2)*(2.d0-2.d0*cos(LLx*(X(i,1)-X(j,1))))&
                              +LLy**(-2)*(2.d0-2.d0*cos(LLy*(X(i,2)-X(j,2))))&
                              +LLz**(-2)*(2.d0-2.d0*cos(LLz*(X(i,3)-X(j,3)))))**(0.5d0)
        end do 
    end do
    
    end if
    
    
    write(file_index,'(a)') "____________________________________________________________________________________________"
    write(file_index,'(a)') ""
    write(file_index,'(a)') '                                    The distance matrix (Euclidean)' 
    write(file_index,'(a)')
    write(file_index,'(a)') ""
    
    call matout(N,N,X_distance,file_index)
  
  endsubroutine
  
  subroutine anim(N,D,X,E,iter,or,Lx,Ly,Lz)
  
  ! Input variables

  integer,intent(in)            :: N , D , iter
  double precision ,intent(in)  :: X(N,D)
  double precision ,intent(in)  :: E
  double precision ,intent(in)  :: Lx,Ly,Lz
  character(len=20),intent(in)  :: or

  ! local

  integer                       :: i
  double precision              :: Xb(N,D)
  
  
  
  if (iter .ne. 1) then

  if     (D == 3) then

    if (or == "origin") then
      Xb(:,1) = X(:,1) - X(1,1)
      Xb(:,2) = X(:,2) - X(1,2)
      Xb(:,3) = X(:,3) - X(1,3)
      do i = 1 , N
        if (Xb(i,1) < 0)   Xb(i,1) = Xb(i,1) + Lx
        if (Xb(i,1) >= Lx) Xb(i,1) = Xb(i,1) - Lx
        if (Xb(i,2) < 0)   Xb(i,2) = Xb(i,2) + Ly
        if (Xb(i,2) >= Ly) Xb(i,2) = Xb(i,2) - Ly
        if (Xb(i,3) < 0)   Xb(i,3) = Xb(i,3) + Lz
        if (Xb(i,3) >= Lz) Xb(i,3) = Xb(i,3) - Lz
      end do
      write(4,'(10000(1x,f12.8))') Xb(:, 1),Xb(:, 2),Xb(:,3)
      write(8,'(f24.18)') E
      else
      write(4,'(10000(1x,f12.8))') X(:, 1),X(:, 2),X(:,3)
      write(8,'(f24.18)') E
    end if

  elseif (D == 2) then
    if (or == "origin") then
      Xb(:,1) = X(:,1) - X(1,1)
      Xb(:,2) = X(:,2) - X(1,2)
    do i = 1 , N
        if (Xb(i,1) < 0)   Xb(i,1) = Xb(i,1) + Lx
        if (Xb(i,1) >= Lx) Xb(i,1) = Xb(i,1) - Lx
        if (Xb(i,2) < 0)   Xb(i,2) = Xb(i,2) + Ly
        if (Xb(i,2) >= Ly) Xb(i,2) = Xb(i,2) - Ly
    end do
    write(4,'(10000(1x,f12.8))') Xb(:, 1),Xb(:, 2)
    write(8,'(f24.18)') E
    else
    write(4,'(10000(1x,f12.8))') X(:, 1),X(:, 2)
    write(8,'(f24.18)') E
    end if

  elseif (D == 1) then
    if (or == "origin") then
      Xb(:,1) = X(:,1) - X(1,1)
    do i = 1, N
      if (Xb(i,1) < 0)   Xb(i,1) = Xb(i,1) + Lx
      if (Xb(i,1) >= Lx) Xb(i,1) = Xb(i,1) - Lx
    end do
    write(4,'(10000(1x,f12.8))') Xb(:, 1)
    write(8,'(f24.18)') E
    else
    write(4,'(10000(1x,f12.8))') X(:, 1)
    write(8,'(f20.12)') E
    end if

  end if
  else
  if     (D == 3) then
    write(4,'(10000(1x,f16.8))') X(:, 1),X(:, 2),X(:,3)
    write(8,'(f24.18)') E
  elseif (D == 2) then
    write(4,'(10000(1x,f16.8))') X(:, 1),X(:, 2)
    write(8,'(f24.18)') E
  elseif (D == 1) then
    write(4,'(10000(1x,f16.8))') X(:, 1)
    write(8,'(f24.18)') E
  end if
  end if

  end subroutine
  
  subroutine plot_anim(N,D,iter,Lx,Ly,Lz,file_index)
  
  implicit none 
  
  ! Input variables

  integer,intent(in)            :: N , D , iter 
  integer,intent(in)            :: file_index
  double precision, intent(in)  :: Lx, Ly , Lz 
  character(len=25)             :: name1,name2
  integer                       :: i , b 
  
  
  if (iter .ne. 1) then
    write(file_index,'(a)') "____________________________________________________________________________________________"
    write(file_index,'(a)') "" 
    write(file_index,'(a)') "Please wait for the animation"
    write(file_index,'(a)') "____________________________________________________________________________________________"
    write(file_index,'(a)') ""
!    open (9,file='plot',status = 'replace')
    write(9,'(a)') 'set term png size 1920,1080'
    write(9,'(a)') 'system "mkdir tmp"'
    write(9,'(a)') "getValue(row,col,filename) = system('awk ''{if (NR == '.row.') print&
                    & $'.col.'}'' '.filename.'')"
    write(9,'(a,f16.10,a)') 'set xrange [0:',Lx,']'
    write(9,'(a,f16.10,a)') 'set yrange [0:',Ly,']'
    write(9,'(a,f16.10,a)') 'set zrange [0:',Lz,']'
    write(9,'(a,f16.10,a)') 'set xtics ("0" 0,"L_x"',Lx,')'
    write(9,'(a,f16.10,a)') 'set ytics       ("L_y"',Ly,')'
    write(9,'(a,f16.10,a)') 'set ztics ("0" 0,"L_z"',Lz,')'
    if      (D == 3) then 
      write(9,'(a)') 'set xtics offset 1,-1,0'
      write(9,'(a)') 'set ytics offset 1,-1,0'
      write(9,'(a)') 'set ztics offset 0,0,0'
      write(9,'(a)') 'set xyplane relative 0'
      write(9,'(a)') 'set view equal xyz'
      write(9,'(a)') 'set border 4095'
    else if (D == 2) then 
      write(9,'(a)') 'set size ratio -1'
    else if (D == 1) then
      write(9,'(a)') 'set size square'
      write(9,'(a)') 'set border 1'
      write(9,'(a)') 'unset ytics'
      write(9,'(a)') 'set xtics nomirror'
      write(9,'(a)') 'set yrange [0:1]'
    end if  
    write(9,'(a)') 'set grid' 
    write(9,'(a,I4)') 'N=' , N
    if (iter > 1000) then 
    b = 1000 
    else 
    b = iter -2  
    end if 
    write(9,'(a,I5,a)') 'do for [i=1:',b,']{'
    write(9,'(a)') 'set output "tmp/image.".i.".png"'
    write(9,'(a)') "x = getValue(i,1,'energy.dat')"
    
    if      (D == 3) then 
      write(9,'(a,f16.10,a,f16.10,a,f16.10,a)') "set label sprintf('Energy = %16.10f', x*1.0)&
                                                 &at screen 0.8,0.9  center font ',18'"
      if (iter > 1000) then 
        write(9,'(a,I4)') "set view 75, i*360/",b
      else 
        write(9,'(a,I4)') "set view 75, i*360/",b+360
      end if 
      write(9,'(a,I4,a,I4,a,I4,a)') "splot for [j=1:",N,"] 'data_frame.dat'&
                                     & u (column(j)):(column(",N,"+j)):(column(",2*N,"+j)) every ::i::i w p pt 7 ps 3 notitle"
    else if (D == 2) then 
      write(9,'(a)') "set label sprintf('Energy = %16.10f', x*1.0) at screen 0.9,0.9  center font ',18'"
      write(9,'(a,I4,a,I4,a)')      "p for [j=1:",N,"] 'data_frame.dat'&
                                     & u (column(j)):(column(",N,"+j)) every ::i::i w p pt 7 ps 3 notitle"
    else if (D == 1 ) then 
      write(9,'(a)') "set label sprintf('Energy = %16.10f', x*1.0) at screen 0.5,0.9  center font ',18'"
      write(9,'(a,I4,a,I4,a)')      "p for [j=1:",N,"] 'data_frame.dat'&
                                     & u j:(0) every ::i::i w p pt 7 ps 3 notitle"
    end if 
      write(9,'(a)') "clear"
      write(9,'(a)') "unset label"
      write(9,'(a)') "}"
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      write(name1,'(I6,a,I6,a)') D,'D',N,'.mp4'
      name2 = ""
      do i=1,len(name1)   
        if (name1(i:i).ne.' ')name2=trim(name2)//trim(name1(i:i))   
      end do
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      write(9,'(a,a)') "system 'ffmpeg -y -nostats -loglevel 0 -f image2&
                      & -r 30.0 -i tmp/image.%d.png -pix_fmt yuv420p -crf 1 ",name2
      write(9,'(a)') "system 'rm -rf tmp/'"

    else
        
        if      (D == 3) then 
        
        write(file_index,'(a)') "____________________________________________________________________________________________"
        write(file_index,'(a)') "" 
        write(file_index,'(a)') "Please wait for the animation"
        write(file_index,'(a)') "____________________________________________________________________________________________"
        write(file_index,'(a)') ""
        write(9,'(a)') 'set term png size 1920,1080'
        write(9,'(a)') 'system "mkdir tmp"'
        write(9,'(a)') "getValue(row,col,filename) = system('awk ''{if (NR == '.row.') print&
                        & $'.col.'}'' '.filename.'')"
        write(9,'(a,f16.10,a)') 'set xrange [0:',Lx,']'
        write(9,'(a,f16.10,a)') 'set yrange [0:',Ly,']'
        write(9,'(a,f16.10,a)') 'set zrange [0:',Lz,']'
        write(9,'(a,f16.10,a)') 'set xtics ("0" 0,"L_x"',Lx,')'
        write(9,'(a,f16.10,a)') 'set ytics       ("L_y"',Ly,')'
        write(9,'(a,f16.10,a)') 'set ztics ("0" 0,"L_z"',Lz,')'
        write(9,'(a)') 'set xtics offset 1,-1,0'
        write(9,'(a)') 'set ytics offset 1,-1,0'
        write(9,'(a)') 'set ztics offset 0,0,0'
        write(9,'(a)') 'set xyplane relative 0'
        write(9,'(a)') 'set view equal xyz'
        write(9,'(a)') 'set border 4095'
        write(9,'(a)') 'set grid'
        write(9,'(a,I4)') 'N=' , N
        write(9,'(a,I5,a)') 'do for [i=1:',300,']{'
        write(9,'(a)') 'set output "tmp/image.".i.".png"'
        write(9,'(a)') "x = getValue(1,1,'energy.dat')"
        
        write(9,'(a,f16.10,a,f16.10,a,f16.10,a)') "set label sprintf('Energy = %16.10f', x*1.0)&
                                                 &at screen 0.8,0.9  center font ',18'"
        write(9,'(a,I4)') "set view 75, i*360/",300                                         
        write(9,'(a,I4,a,I4,a,I4,a)') "splot for [j=1:",N,"] 'data_frame.dat'&
                              & u (column(j)):(column(",N,"+j)):(column(",2*N,"+j)) w p pt 7 ps 3 notitle"
        write(9,'(a)') "clear"
        write(9,'(a)') "unset label"
        write(9,'(a)') "}"
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        write(name1,'(I6,a,I6,a)') D,'D',N,'.mp4'
        name2 = ""
        do i=1,len(name1)   
          if (name1(i:i).ne.' ')name2=trim(name2)//trim(name1(i:i))   
        end do
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        write(9,'(a,a)') "system 'ffmpeg -y -nostats -loglevel 0 -f image2&
                        & -r 30.0 -i tmp/image.%d.png -pix_fmt yuv420p -crf 1 ",name2
        write(9,'(a)') "system 'rm -rf tmp/'"
        
        else if (D == 2) then 
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        write(name1,'(a,I6,a,I6,a)') '"',D,'D',N,'.png"'
        name2 = ""
        do i=1,len(name1)   
          if (name1(i:i).ne.' ')name2=trim(name2)//trim(name1(i:i))   
        end do
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        write(file_index,'(a)') "____________________________________________________________________________________________"
        write(file_index,'(a)') ""
        write(file_index,'(a)') 'I can not provide you animation for one frame but i can give you a pic as png'
        write(file_index,'(a)') "____________________________________________________________________________________________"
        write(file_index,'(a)') ""
        write(9,'(a)') 'set term png size 1920,1080'
        write(9,'(a,f16.10,a)') 'set xrange [0:',Lx,']'
        write(9,'(a,f16.10,a)') 'set yrange [0:',Ly,']'
        write(9,'(a,f16.10,a)') 'set zrange [0:',Lz,']'
        write(9,'(a,f16.10,a)') 'set xtics ("0" 0,"L_x"',Lx,')'
        write(9,'(a,f16.10,a)') 'set ytics       ("L_y"',Ly,')'
        write(9,'(a,f16.10,a)') 'set ztics ("0" 0,"L_z"',Lz,')'
         write(9,'(a)') 'set size ratio -1'
        write(9,'(a)') 'set grid'
        write(9,'(a,I4)') 'N=' , N
        write(9,'(a,a)') 'set output ',name2
        write(9,'(a)') "getValue(row,col,filename) = system('awk ''{if (NR == '.row.') print&
                        &$'.col.'}'' '.filename.'')"
        write(9,'(a)') "x = getValue(1,1,'energy.dat')"
        
        write(9,'(a)') "set label sprintf('Energy = %16.10f', x*1.0)&
                                                 &at screen 0.9,0.9  center font ',18'"
        write(9,'(a,I4,a,I4,a,I4,a)') "plot for [j=1:",N,"] 'data_frame.dat' u (column(j)):(column(",N,"+j)) w p pt 7 ps 3 notitle"
        else if (D == 1 ) then 
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        write(name1,'(a,I6,a,I6,a)') '"',D,'D',N,'.png"'
        name2 = ""
        do i=1,len(name1)   
          if (name1(i:i).ne.' ')name2=trim(name2)//trim(name1(i:i))   
        end do
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        write(file_index,'(a)') "____________________________________________________________________________________________"
        write(file_index,'(a)') ""
        write(file_index,'(a)') 'I can not provide you animation for one frame but i can give you a pic as png'
        write(file_index,'(a)') "____________________________________________________________________________________________"
        write(file_index,'(a)') ""
        write(9,'(a)') 'set term png size 1920,1080'
        write(9,'(a,f16.10,a)') 'set xrange [0:',Lx,']'
        write(9,'(a,f16.10,a)') 'set yrange [0:',Ly,']'
        write(9,'(a,f16.10,a)') 'set zrange [0:',Lz,']'
        write(9,'(a,f16.10,a)') 'set xtics ("0" 0,"L_x"',Lx,')'
        write(9,'(a,f16.10,a)') 'set ytics       ("L_y"',Ly,')'
        write(9,'(a,f16.10,a)') 'set ztics ("0" 0,"L_z"',Lz,')'
        write(9,'(a)') 'set size square'
        write(9,'(a)') 'set border 1'
        write(9,'(a)') 'unset ytics'
        write(9,'(a)') 'set xtics nomirror'
        write(9,'(a)') 'set yrange [0:1]'
        write(9,'(a)') 'set grid'
        write(9,'(a,I4)') 'N=' , N
        write(9,'(a,a)') "set output ",name2
        write(9,'(a)') "getValue(row,col,filename) = system('awk ''{if (NR == '.row.') print&
                        &$'.col.'}'' '.filename.'')"
        write(9,'(a)') "x = getValue(1,1,'energy.dat')"
        
        
        write(9,'(a,a)') "set label sprintf('Energy = %16.10f', x*1.0) at screen 0.5,0.9  center font ',18'"
        write(9,'(a,I4,a,I4,a,I4,a)') "plot for [j=1:",N,"] 'data_frame.dat'&
                                       &u j:(0) w p pt 7 ps 3 notitle"
        end if 
  end if  
  
  
  end subroutine

subroutine matout(m,n,A,file_index)

! Print the MxN array A

  implicit none

  integer,parameter             :: ncol = 5
  double precision,parameter    :: small = 1d-10
  integer,intent(in)            :: m,n
  integer,intent(in)            :: file_index
  double precision,intent(in)   :: A(m,n)
  double precision              :: B(ncol)
  integer                       :: ilower,iupper,num,i,j
  
  do ilower=1,n,ncol
    iupper = min(ilower + ncol - 1,n)
    num = iupper - ilower + 1
    write(file_index,'(3X,10(9X,I6))')
    write(file_index,'(3X,10(9X,I6))') (j,j=ilower,iupper)
    write(file_index,'(3X,10(9X,I6))')
    do i=1,m
      do j=ilower,iupper
        B(j-ilower+1) = A(i,j)
      enddo
      do j=1,num
        if(abs(B(j)) < small) B(j) = 0d0
      enddo
      write(file_index,'(I7,10F15.8)') i,(B(j),j=1,num)
    enddo
  enddo

end subroutine matout