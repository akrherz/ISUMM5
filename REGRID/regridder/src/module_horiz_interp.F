MODULE horiz_interp

CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   FUNCTION bint(xx,yy,list,iii,jjj,ibint)
   
      !  Bi-linear interpolation among four grid values
   
      IMPLICIT NONE
   
      REAL :: xx , yy
      INTEGER :: ibint , iii, jjj
      REAL list(iii,jjj),stl(4,4)
   
      INTEGER :: ib , jb, n , i , j , k , kk , l , ll
      REAL :: bint , x , y , a , b , c , d , e , f , g , h
#ifdef crayx1
      REAL :: a2(4)
#endif
   
      ib=iii-ibint
      jb=jjj-ibint
      bint = 0.0
      n = 0
      i = INT(xx+0.00001)
      j = INT(yy+0.00001)
      x = xx - i
      y = yy-j
   
      IF ( ( ABS(x).GT.0.0001 ) .OR. ( abs(y).gt.0.0001 ) ) THEN
         loop_1 : DO k = 1,4
            kk = i + k - 2
            IF ( kk .LT. 1) then 
               kk = 1
            END IF
            IF ( kk .GT. ib) THEN
               kk = ib
            END IF
            loop_2 : DO l = 1,4
               stl(k,l) = 0.
               ll = j + l - 2
               IF ( ll .LT. 1) THEN
                  ll = 1
               END IF
               IF ( ll .GT. jb) THEN
                  ll = jb
               END IF
               stl(k,l) = list(kk,ll)
               n = n + 1
               IF ( stl(k,l) .EQ. 0. ) THEN
                  stl(k,l) = 1.E-20
               END IF
            END DO loop_2
         END DO loop_1
   
#ifdef crayx1
! pjj/cray - vectorize
         do k=1,4
            a2(k) = oned(x, stl(1,k), stl(2,k), stl(3,k), stl(4,k))
         enddo
         bint = oned(y,a2(1),a2(2),a2(3),a2(4))
#else
         a = oned(x,stl(1,1),stl(2,1),stl(3,1),stl(4,1))
         b = oned(x,stl(1,2),stl(2,2),stl(3,2),stl(4,2))
         c = oned(x,stl(1,3),stl(2,3),stl(3,3),stl(4,3))
         d = oned(x,stl(1,4),stl(2,4),stl(3,4),stl(4,4))
         bint = oned(y,a,b,c,d)
#endif
   
         IF(n.NE.16) THEN
#ifdef crayx1
            do k=1,4
               a2(k) = oned(y,stl(k,1),stl(k,2),stl(k,3),stl(k,4))
            enddo
            bint = (bint+oned(x,a2(1),a2(2),a2(3),a2(4))) * 0.5
#else
            e = oned(y,stl(1,1),stl(1,2),stl(1,3),stl(1,4))
            f = oned(y,stl(2,1),stl(2,2),stl(2,3),stl(2,4))
            g = oned(y,stl(3,1),stl(3,2),stl(3,3),stl(3,4))
            h = oned(y,stl(4,1),stl(4,2),stl(4,3),stl(4,4))
            bint = (bint+oned(x,e,f,g,h)) * 0.5
#endif
         END IF
   
      ELSE
         bint = list(i,j)
      END IF
   
   END FUNCTION bint

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   FUNCTION oned(x,a,b,c,d) 
   
      IMPLICIT NONE
   
      REAL :: x,a,b,c,d,oned
   
      oned = 0.                
   
      IF      ( x .EQ. 0. ) THEN
         oned = b      
      ELSE IF ( x .EQ. 1. ) THEN
         oned = c      
      END IF
   
      IF(b*c.NE.0.) THEN
         IF ( a*d .EQ. 0. ) THEN
            IF      ( ( a .EQ. 0 ) .AND. ( d .EQ. 0 ) ) THEN
               oned = b*(1.0-x)+c*x                                        
            ELSE IF ( a .NE. 0. ) THEN
               oned = b+x*(0.5*(c-a)+x*(0.5*(c+a)-b))            
            ELSE IF ( d .NE. 0. ) THEN
               oned = c+(1.0-x)*(0.5*(b-d)+(1.0-x)*(0.5*(b+d)-c)) 
            END IF
         ELSE
            oned = (1.0-x)*(b+x*(0.5*(c-a)+x*(0.5*(c+a)-b)))+x*(c+(1.0-x)*(0.5*(b-d)+(1.0-x)*(0.5*(b+d)-c)))                                   
         END IF
      END IF
   
   END FUNCTION oned                                                       

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


END MODULE horiz_interp
