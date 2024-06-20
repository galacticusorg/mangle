c-----------------------------------------------------------------------
c � A J S Hamilton 2001
c-----------------------------------------------------------------------
      subroutine rrderiv(nd,phi,a,da)
      integer, parameter ::  star10 = selected_real_kind(r=4931,p=18)
      common /rrdervc/ czeta,szeta,cm1,cm2,
c *
c * Integrand of angular pair integral
c *
      sphi=sin(phi)
      cphi=cos(phi)
      cn1z2=cth1*czeta+sth1*szeta*cphi
      sn1z2=1-cn1z2**2
      if (sn1z2.le.0._star10) then
        da=0._star10
      else
        sn1z2=sqrt(sn1z2)
      endif
