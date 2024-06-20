c-----------------------------------------------------------------------
c © A J S Hamilton 2001
c-----------------------------------------------------------------------
      subroutine gphim(angle,rp,cm,np,rpi,cmi,cmimin,cmimax,tol,
     *  phi,iord)
      integer, parameter ::  star10 = selected_real_kind(r=4931,p=18)
      integer np,iord(2*np)
      real(kind=star10) angle,rp(3,np),cm(np),rpi(3),cmi,cmimin,cmimax,tol,
     *  phi(2,np)
c
c        parameters
      include 'pi.par'
      real(kind=star10) TWOPI
      parameter (TWOPI=2._star10*PI)
c        intrinsics
      intrinsic abs
c *
c * Same as gphi, but speed up matters by checking first whether cmi
c * lies outside |cmimin| and |cmimax|, giving angle of 0 or 2*pi.
c * cmimin and cmimax are gotten from prior call to gcmlim.
c *
      if (cmi.le.abs(cmimin)) then
c        region excludes circle
        if (cmimin.ge.0._star10) then
          angle=0._star10
c        region encloses circle
        elseif (cmimin.lt.0._star10) then
          angle=TWOPI
        endif
      elseif (cmi.ge.abs(cmimax)) then
c        circle encloses region
        if (cmimax.ge.0._star10) then
          angle=0._star10
c        circle and region enclose each other
        elseif (cmimax.lt.0._star10) then
          angle=TWOPI
        endif
      else
c        circle intersects boundary of region
        call gphi(angle,rp,cm,np,rpi,cmi,phi,tol,iord)
      endif
      return
      end
c
