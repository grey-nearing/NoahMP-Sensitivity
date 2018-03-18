subroutine enks(X,Y,Zbar,Zsig,Nt,Ne,Dx,Dz)
 implicit none
 
! in/out 
 integer, intent(in)                      :: Nt, Ne, Dz, Dx
 real, dimension(Dz), intent(in)          :: Zbar
 real, dimension(Dz), intent(in)          :: Zsig
 real, dimension(Nt,Dx,Ne), intent(inout) :: X
 real, dimension(Dz,Ne), intent(in)       :: Y

! constants
 real, dimension(Dz,Dz) :: eyeZ

! manipulation vectors
 real, dimension(Dx,Ne) :: Xbar, KK
 real, dimension(Dz,Ne) :: Ybar
 real, dimension(Dz,Ne) :: Z
 real, dimension(Dz,Dz) :: R, Cyy, Qinv
 real, dimension(Dx,Dz) :: Cxy, K
 real, dimension(Ne,Ne) :: X5, eyeE
 real, dimension(Ne,Dz) :: X4

 real, dimension(Dx,Dx) :: Cxx
 real, dimension(Dx,Dz) :: CC

 real, dimension(Nt,Dx,Ne) :: temp

! indexes
 integer e, t, d, i
 integer, dimension(Dz) :: ipiv
 integer :: info
 real :: eta, random_normal

! identity matrix
 eyeZ = 0.
 do d = 1,Dz
  eyeZ(d,d) = 1.
 enddo
 eyeE = 0.
 do d = 1,Ne
  eyeE(d,d) = 1.
 enddo

! sample observation
 do e = 1,Ne 
  do d = 1,Dz
   eta = random_normal()
!   Z(d,e) = Zbar(d) * (1 + eta*Zbar(d)*Zsig(d))
   Z(d,e) = Zbar(d) + eta*Zsig(d)
  enddo
 enddo

!print*, Y
!print*, '---------------'
!print*, Z
!print*, '---------------'
!print*, X(1,1,:)
!print*, '---------------'
!print*, X(1,2,:)
!print*, '---------------'
!print*, X(1,3,:)
!print*, '---------------'

! compute bar matrices
 do d = 1,Dz
  Ybar(d,:) = Y(d,:) - sum(Y(d,:))/Ne
 enddo
 do d = 1,Dx
  Xbar(d,:) = X(1,d,:) - sum(X(1,d,:))/Ne
 enddo

! compute cross-covs
 Cyy = 0.
 do d = 1,Dz
  do e = 1,Dz
   do i = 1,Ne 
    Cyy(d,e) = Cyy(d,e) + Ybar(d,i)*Ybar(e,i)
   enddo
   Cyy(d,e) = Cyy(d,e)/(Ne-1) 
  enddo
 enddo

 Cxy = 0.
 do d = 1,Dx
  do e = 1,Dz
   do i = 1,Ne
    Cxy(d,e) = Cxy(d,e) + Xbar(d,i)*Ybar(e,i)
   enddo
   Cxy(d,e) = Cxy(d,e)/(Ne-1)
  enddo
 enddo

 R = 0.
 do d = 1,Dz
  R(d,d) = Zsig(d)**2
 enddo

 Cxx = 0.
 do d = 1,Dx
  do e = 1,Dx
   do i = 1,Ne
    Cxx(d,e) = Cxx(d,e) + Xbar(d,i)*Xbar(e,i)
   enddo
   Cxx(d,e) = Cxx(d,e)/(Ne-1)
  enddo   
 enddo

 do d = 1,Dx
  do e = 1,Dz
   CC(d,e) = Cxy(d,e)/sqrt(Cxx(d,d))/sqrt(Cyy(e,e))
  enddo
 enddo 

! invert covariance matrix
 Qinv = Cyy+R
 call sgesv(Dz,Dz,Qinv,Dz,ipiv,eyeZ,Dz,info)
 Qinv = eyeZ

! compute Kalman Gain
 Z = Z-Y

 K = matmul(Cxy,Qinv)     
 do d = 1,Dx
  do e = 1,Dz
   if (abs(CC(d,e)).lt.0.7) K(d,e) = 0.
  enddo
 enddo
 KK = matmul(K,Z) 

 !X4 = matmul(transpose(Ybar),Qinv)
 !X5 = matmul(X4,Z)/(Ne-1)

!print*, 'Cxx', Cxx
!print*, 'Cyy', Cyy
!print*, 'Cxy', Cxy
!print*, 'R', R
!print*, 'X', sum(X(1,1,:))/Ne

! preform update	 
 X(1,:,:) = X(1,:,:) + KK 

!print*, 'X', sum(X(1,1,:))/Ne
!stop


! do t = 1,Nt
!  do d = 1,Dx
!   Xbar(d,:) = X(t,d,:) - sum(X(t,d,:))/Ne
!  enddo
!  X(t,:,:) = X(t,:,:) + matmul(Xbar,X5) 
! enddo


return
end subroutine enks





