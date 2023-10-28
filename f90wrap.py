from __future__ import print_function, absolute_import, division
import _f90wrap
import f90wrap.runtime
import logging
import numpy

class Outptgen(f90wrap.runtime.FortranModule):
    """
    Module outptgen
    
    
    Defined at outputgen.f90 lines 1-73
    
    """
    @property
    def k(self):
        """
        Element k ftype=integer(kind=4) pytype=int
        
        
        Defined at outputgen.f90 line 9
        
        """
        return _f90wrap.f90wrap_outptgen__get__k()
    
    @k.setter
    def k(self, k):
        _f90wrap.f90wrap_outptgen__set__k(k)
    
    @property
    def k2(self):
        """
        Element k2 ftype=integer(kind=4) pytype=int
        
        
        Defined at outputgen.f90 line 9
        
        """
        return _f90wrap.f90wrap_outptgen__get__k2()
    
    @k2.setter
    def k2(self, k2):
        _f90wrap.f90wrap_outptgen__set__k2(k2)
    
    @property
    def k1(self):
        """
        Element k1 ftype=integer(kind=4) pytype=int
        
        
        Defined at outputgen.f90 line 9
        
        """
        return _f90wrap.f90wrap_outptgen__get__k1()
    
    @k1.setter
    def k1(self, k1):
        _f90wrap.f90wrap_outptgen__set__k1(k1)
    
    @property
    def klc(self):
        """
        Element klc ftype=integer(kind=4) pytype=int
        
        
        Defined at outputgen.f90 line 9
        
        """
        return _f90wrap.f90wrap_outptgen__get__klc()
    
    @klc.setter
    def klc(self, klc):
        _f90wrap.f90wrap_outptgen__set__klc(klc)
    
    @property
    def i(self):
        """
        Element i ftype=integer(kind=4) pytype=int
        
        
        Defined at outputgen.f90 line 10
        
        """
        return _f90wrap.f90wrap_outptgen__get__i()
    
    @i.setter
    def i(self, i):
        _f90wrap.f90wrap_outptgen__set__i(i)
    
    @property
    def j(self):
        """
        Element j ftype=integer(kind=4) pytype=int
        
        
        Defined at outputgen.f90 line 10
        
        """
        return _f90wrap.f90wrap_outptgen__get__j()
    
    @j.setter
    def j(self, j):
        _f90wrap.f90wrap_outptgen__set__j(j)
    
    @property
    def nel(self):
        """
        Element nel ftype=integer(kind=4) pytype=int
        
        
        Defined at outputgen.f90 line 10
        
        """
        return _f90wrap.f90wrap_outptgen__get__nel()
    
    @nel.setter
    def nel(self, nel):
        _f90wrap.f90wrap_outptgen__set__nel(nel)
    
    @property
    def j1(self):
        """
        Element j1 ftype=integer(kind=4) pytype=int
        
        
        Defined at outputgen.f90 line 10
        
        """
        return _f90wrap.f90wrap_outptgen__get__j1()
    
    @j1.setter
    def j1(self, j1):
        _f90wrap.f90wrap_outptgen__set__j1(j1)
    
    @property
    def l1(self):
        """
        Element l1 ftype=integer(kind=4) pytype=int
        
        
        Defined at outputgen.f90 line 10
        
        """
        return _f90wrap.f90wrap_outptgen__get__l1()
    
    @l1.setter
    def l1(self, l1):
        _f90wrap.f90wrap_outptgen__set__l1(l1)
    
    @property
    def no(self):
        """
        Element no ftype=integer(kind=4) pytype=int
        
        
        Defined at outputgen.f90 line 10
        
        """
        return _f90wrap.f90wrap_outptgen__get__no()
    
    @no.setter
    def no(self, no):
        _f90wrap.f90wrap_outptgen__set__no(no)
    
    @property
    def n1(self):
        """
        Element n1 ftype=integer(kind=4) pytype=int
        
        
        Defined at outputgen.f90 line 10
        
        """
        return _f90wrap.f90wrap_outptgen__get__n1()
    
    @n1.setter
    def n1(self, n1):
        _f90wrap.f90wrap_outptgen__set__n1(n1)
    
    @property
    def dat(self):
        """
        Element dat ftype=character(len=8) pytype=str
        
        
        Defined at outputgen.f90 line 12
        
        """
        array_ndim, array_type, array_shape, array_handle = \
            _f90wrap.f90wrap_outptgen__array__dat(f90wrap.runtime.empty_handle)
        if array_handle in self._arrays:
            dat = self._arrays[array_handle]
        else:
            dat = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                    f90wrap.runtime.empty_handle,
                                    _f90wrap.f90wrap_outptgen__array__dat)
            self._arrays[array_handle] = dat
        return dat
    
    @dat.setter
    def dat(self, dat):
        self.dat[...] = dat
    
    def __str__(self):
        ret = ['<outptgen>{\n']
        ret.append('    k : ')
        ret.append(repr(self.k))
        ret.append(',\n    k2 : ')
        ret.append(repr(self.k2))
        ret.append(',\n    k1 : ')
        ret.append(repr(self.k1))
        ret.append(',\n    klc : ')
        ret.append(repr(self.klc))
        ret.append(',\n    i : ')
        ret.append(repr(self.i))
        ret.append(',\n    j : ')
        ret.append(repr(self.j))
        ret.append(',\n    nel : ')
        ret.append(repr(self.nel))
        ret.append(',\n    j1 : ')
        ret.append(repr(self.j1))
        ret.append(',\n    l1 : ')
        ret.append(repr(self.l1))
        ret.append(',\n    no : ')
        ret.append(repr(self.no))
        ret.append(',\n    n1 : ')
        ret.append(repr(self.n1))
        ret.append(',\n    dat : ')
        ret.append(repr(self.dat))
        ret.append('}')
        return ''.join(ret)
    
    _dt_array_initialisers = []
    

outptgen = Outptgen()

