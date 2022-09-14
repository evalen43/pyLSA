import math
import sqlite3
import xml.etree.ElementTree as ET
from os.path import abspath, expanduser

import numpy as np
import wx.xml

#import scipy
#import wx.dataview

class Unit:
    
    @staticmethod
    def TokNperM2(unitF):
        if unitF == "kN/m2":
            tonewton=1.0
        elif unitF =="lb/sqf":
            tonewton = 0.047880258888889
        elif unitF=="ksi":
            tonewton = 6894.757280000001
        elif unitF=="kgf/m2":
            tonewton=StruMod.grav
        elif unitF=="ton/sqf":
            tonewton = 107.25177991111
        elif unitF=="psi":
            tonewton = 6.89475728
        else:
            tonewton=1.0
        return tonewton        

    @staticmethod
    def TokNperM3(unitF):
        if unitF=="kN/m3":
            tonewton=1.0
        elif unitF=="lbf/ft3":
            tonewton = 0.1570865731
        elif unitF=="kips/inch3":
            tonewton = 271447.14116097
        elif unitF=="kgf/m3":
            tonewton=StruMod.grav/1000.0
        elif unitF=="tonf/ft3":
            tonewton = 314.1731461238
        elif unitF=="t/m3":
            tonewton=StruMod.grav
        else:
            tonewton=1.0
        return tonewton            

    @staticmethod
    def ToMeter(unitL):
        if unitL=="inch":
            factor=0.0254
        elif unitL=="feet":
            factor=0.3048
        elif unitL=="cm":
            factor=0.01
        elif unitL=="m":
            factor=1.0
        return factor

    @staticmethod
    def TokN(unitF):
        if unitF == 'lbf':
            factor = 0.454*StruMod.grav/1000.0
        elif unitF == 'kip':
            factor = 0.454*StruMod.grav
        elif unitF == 'N':
            factor = 0.001
        elif unitF == 'te':
            factor = 1.0*StruMod.grav
        return factor    

class StruMod(Unit):
    grav = 9.806
    ndf = 0
    ne = 0
    ms = 0
    n = 0
    nn=0
    nne = 2
    nbn=0
    ndfel=0
    nlc=0
    nmat=0
    nsec=0
    nlnodes=0
    nlmem=0
    fyield = 0.0

    strutype = ''
    projName=''
    exampletitle=''
    materials = []
    sections = []

    x = []
    y = []
    z = []
    nodes = []
    coor = []
    nodelist = []
    seclist = []
    matlist = []
    elemlist = []
    elem_prop = []
    bndlist = []
    boundaries = []
    
    nodeloads = []
    memloads=[]
    loadcaseslist = []
    elem_prop_arr = np.zeros((1, 1))
    sections_arr=np.zeros((1,1))
    mat_table=np.zeros((1,1))
    al=np.zeros((1,1))
    reac=np.zeros((1,1))
    ib=np.zeros((1), dtype=int)
    mfem_param=np.zeros((1,1))
    node_list=np.empty((1,10), dtype='S10')

    @staticmethod
    def pipeparam(od,wth):
        id=od-2*wth
        ax=0.25*math.pi*(od**2-id**2)
        iner=math.pi*(od**4-id**4)/64
        radius=math.sqrt(iner/ax)
        Zx=0.0
        Sx=iner*2/od
        w=0.0
        J=2*iner
        section=(w,ax,od,iner,Zx,Sx,radius,iner,Zx,Sx,radius,J)
        return section  
    
    @classmethod
    def bndparam(cls,bndid,bntype):
        j=cls.nodelist.index(bndid)
        if bntype=='ENCASTRE': ibnd=(j+1,0,0,0)
            #ib.append(ibnd)
        elif bntype=='HINGE':  ibnd=(j+1,0,0,1)
            #ib.append(ibnd)
        elif bntype=='HSLIDEX': ibnd=(j+1,1,0,0)
            #ib.append(ibnd)
        elif bntype=='HSLIDEY': ibnd=[j+1,0,1,0]
        return ibnd
    
    @staticmethod
    def structure(strutype):
        if strutype == "Frame2D":   ndf = 3
        elif strutype == "Frame3D": ndf = 6
        elif strutype == "Truss3D":
            ndf = 3
        elif strutype == "Truss2D":
            ndf = 2
        elif strutype == "Grid":
            ndf = 3
        elif strutype == "Frame2D_8DOF":
            ndf = 4
        else: ndf=3 
        return ndf       

    @classmethod
    def code(cls,content,child):
        lineinput=[]
        UnitS = child.GetAttribute("unitS", "kN/m2")
        if UnitS == "default-value": scaleS=1.0
        else: scaleS=Unit.TokNperM2(UnitS)                    
        content=content.replace("="," ")
        content=content.replace("\n"," ")
        lineinput=content.split()
        i=0
        while i<len(lineinput):
            if lineinput[i]=="code":
                Code=lineinput[i+1]
            elif lineinput[i]=="fy":
                fyield = float(lineinput[i+1])*scaleS
            i +=1    
        return Code,fyield

    @staticmethod
    def material(content,child):
        lineinput=[]
        materials=[]
        matlist=[]
        UnitS = child.GetAttribute("unitS", "kN/m2")
        UnitD=child.GetAttribute("den","kN/m3")
        if UnitS == "default-value": scaleS = 1.0
        else: scaleS = Unit.TokNperM2(UnitS)
        if UnitD=="default-value": scaleV=1.0
        else: scaleV=Unit.TokNperM3(UnitD)
        lines=content.splitlines()
        StruMod.nmat=len(lines)     
        for line in lines:
            line = line.replace("=", " ")
            lineinput = line.split()
            i=0
            matlist.append(lineinput[0])
            matid = lineinput[0]
            matype=lineinput[1]
            if matype=='General':
                while i<len(lineinput):
                    if lineinput[i]=="E":
                        emod=float(lineinput[i+1])*scaleS
                    elif lineinput[i]=="Density":
                        matden=float(lineinput[i+1])*scaleV
                    elif lineinput[i]=="Poisson":
                        poisson=float(lineinput[i+1])
                    i +=1
                #material=('',matype,emod,matden,poisson)
                material = (emod, matden, poisson)
                y = list(material)
                #y[0]=matid
                material=tuple(y)
                materials.append(material)
            elif matype=='Steel':
                #steel = ('', 'Steel', 200e+06, 78.5, 0.28)
                steel = (200e+06, 78.5, 0.28)
                y=list(steel)
                #y[0] = matid
                material=tuple(y)
                materials.append(material)
            elif matype=='Titanium':
                #titanium = ('', 'Titaniun', 113e+06, 44.13, 0.3)
                titanium = (113e+06, 44.13, 0.3)
                y = list(titanium)
                #y[0] = matid
                material = tuple(y)
                materials.append(material)
        StruMod.mat_table=np.reshape(materials,newshape=(StruMod.nmat,3))
        return matlist,materials                
    
    @classmethod
    def section(cls,content,child):
        #seclist=[]
        #sections=[]
        UnitL = child.GetAttribute("unitL", "m")
        if UnitL == "default-value":
            scaleL = 1.0
        else:
            scaleL = Unit.ToMeter(UnitL)
        lines = content.splitlines()
        cls.nsec=len(lines)
        for line in lines:
            line = line.replace("=", " ")
            lineinput = line.split()
            i = 0
            cls.seclist.append(lineinput[0])
            secid=lineinput[0]
            sectype = lineinput[1]
            if sectype == 'Tube':
                while i < len(lineinput):
                    if lineinput[i] == 'OD':
                        od = float(lineinput[i+1])*scaleL
                    elif lineinput[i] == 'WTH':
                        wth = float(lineinput[i+1])*scaleL
                    i += 1
                section = StruMod.pipeparam(od, wth)
                #y=list(section)
                #y.insert(0,secid)
                #y.insert(1,"Tube")
                #section=tuple(y)
                #print(section)
                cls.sections.append(tuple(section))
            elif sectype == 'EDI':
                edi = lineinput[2]
                # Connecto to database
                conn = sqlite3.connect(
                    abspath(expanduser('~/pyLSA/aisc_shapes_v15_US_R1.db')))
                # Create cursor
                c = conn.cursor()
                # Query the database
                c.execute("SELECT * FROM shapesv15_US WHERE EDI=edi")
                items = c.fetchall()
                for item in items:
                    if item[0] == edi:
                        y = list(item)
                        y[1] = y[1]*0.01459  # w
                        y[2] = y[2]*scaleL**2  # A
                        y[3] = y[3]*scaleL  # d
                        y[4] = y[4]*scaleL**4  # Ix
                        y[5] = y[5]*scaleL**3  # Zx
                        y[6] = y[6]*scaleL**3  # Sx
                        y[7] = y[7]*scaleL  # rx
                        y[8] = y[8]*scaleL**4  # Iy
                        y[9] = y[9]*scaleL**3  # Zy
                        y[10] = y[10]*scaleL**3  # Sy
                        y[11] = y[11]*scaleL  # ry
                        y[12] = y[12]*scaleL**4  # J
                        y.pop(0)
                        item = tuple(y)
                        cls.sections.append(item)
        cls.sections_arr=np.reshape(cls.sections,newshape=(cls.nsec,12))
        #print(cls.sections_arr)
    @staticmethod
    def nodes(content,child):
        coor=[]
        nodelist=[]
        UnitL = child.GetAttribute("unitL", "m")
        if UnitL == "default-value": scaleL = 1.0
        else: scaleL = Unit.ToMeter(UnitL)
        lines = content.splitlines()
        nn=len(lines)
        n=nn*StruMod.ndf
        for line in lines:
            #line = line.replace("=", " ")
            lineinput = line.split()
            nodelist.append(lineinput[0].ljust(10))
            nodex=float(lineinput[1])*scaleL
            nodey=float(lineinput[2])*scaleL
            if len(lineinput) >3:
                nodez=float(lineinput[3])*scaleL
            else: 
                nodez=0.0
            nodes = (nodex, nodey, nodez)
            coor.append(nodes)
        StruMod.node_list=np.array((StruMod.nn,10))
        for i in range(StruMod.nn):
            StruMod.node_list[i]=nodelist[i]      
        return nn,n,coor,nodelist
    
    @classmethod
    def elem_prop(cls,content,child):
        lines = content.splitlines()
        elem_prop=[]
        ms=0
        StruMod.ne=len(lines)
        for line in lines:
            lineinput = line.split()
            cls.elemlist.append(lineinput[0])
            inc1=cls.nodelist.index(str(lineinput[1]).ljust(10))
            inc2=cls.nodelist.index(str(lineinput[2]).ljust(10))
            j=abs(inc1-inc2)
            if ms<j: ms=j
            secid=cls.seclist.index(lineinput[3])
            matid=cls.matlist.index(lineinput[4])
            pt1=np.array(cls.coor[inc1])
            pt2=np.array(cls.coor[inc2])
            temp=pt1-pt2
            elemlen=np.linalg.norm(temp)
            axis_x=np.array((1,0,0))
            axis_y = np.array((0, 1, 0))
            cosx=np.dot(temp,axis_x.T)/elemlen
            sinx = np.dot(temp, axis_y.T)/elemlen
            element=(inc1+1,inc2+1,secid+1,matid+1,elemlen,cosx,sinx)
            elem_prop.append(element)
        StruMod.ms=cls.ndf*(ms+1)
        StruMod.ndfel=cls.nne*cls.ndf
        cls.elem_prop=elem_prop
        cls.elem_prop_arr=np.reshape(elem_prop,newshape=(cls.ne,7))
        #print(cls.elem_prop_arr)    
    
    @classmethod
    def boundary(cls,content,child):
        lines = content.splitlines()
        cls.nbn=len(lines)
        for line in lines:
            lineinput = line.split()
            bnodeid=str(lineinput[0]).ljust(10)
            #jbn=cls.nodelist.index(bnodeid)
            cls.bndlist.append(bnodeid)
            bntype=lineinput[1]
            cls.boundaries.append(cls.bndparam(bnodeid,bntype))
    
    @classmethod
    def loading(cls,content,child):
        lineinput=[]
        UnitL = child.GetAttribute("unitL", "m")
        if UnitL == "default-value": scaleL = 1.0
        else: scaleL = Unit.ToMeter(UnitL)
        UnitF=child.GetAttribute("unitF","kN")
        if UnitF=="default-value": scaleF=1.0
        else: scaleF=Unit.TokN(UnitF)
        i=0
        load = child.GetChildren()
        while load:
            tagchild=load.GetName()
            tagattrib=load.GetAttribute("id",'')
            if tagchild=='case':
                load2 = load.GetChildren()
                content2 = load2.GetNodeContent()
                tag2=load2.GetName()
                cls.loadcaseslist.append(tagattrib)
                if tag2=='loaded-nodes':
                    lines = content2.splitlines()
                    cls.nlnodes=len(lines)
                    for line in lines:
                        px = 0.0;py = 0.0;mz = 0.0
                        line = line.replace("=", " ")
                        lineinput = line.split()
                        for j in range(len(lineinput)):
                            if  lineinput[j] =='nodeid':
                                nodeid = str(lineinput[j+1]).ljust(10)
                            elif lineinput[j]=='Px': 
                                px=float(lineinput[j+1])*scaleF
                            elif lineinput[j] == 'Py':
                                py = float(lineinput[j+1])*scaleF
                            elif lineinput[j] == 'Mz':
                                mz = float(lineinput[j+1])*scaleL*scaleF
                        k=cls.nodelist.index(nodeid)    
                        ldtuple=(i,k,px,py,mz)
                        cls.nodeloads.append(ldtuple)    
                if tag2 == 'loaded-members':
                    lines = content2.splitlines()
                    cls.nlmem=len(lines)
                    for line in lines:
                        p = 0.0;a = 0.0;mz = 0.0
                        line = line.replace("=", " ")
                        lineinput = line.split()
                        for j in range(len(lineinput)):
                            if lineinput[j]=='memid':
                                memid = lineinput[j+1]
                            elif lineinput[j]=='P': 
                                p=float(lineinput[j+1])*scaleF
                            elif lineinput[j] == 'a':
                                a = float(lineinput[j+1])*scaleL
                            elif lineinput[j] == 'loadtype':
                                ldtype = lineinput[j+1]
                            elif lineinput[j]=='wa': 
                                wa=float(lineinput[j+1])*scaleF
                            elif lineinput[j]=='wb': 
                                wb=float(lineinput[j+1])*scaleF 
                            elif lineinput[j] == 'sysref':
                                sysref = lineinput[j+1]
                                if(sysref=='globx'): kref=1
                                if (sysref == 'globy'):
                                    kref = 2
                                if (sysref == 'globz'):
                                    kref = 3
                        k=cls.elemlist.index(memid)
                        ltype=0
                        if(ldtype=='pload'):
                            ltype=2
                            ldtuple=(ltype,i+1,k+1,p,a,0,kref)                            
                        if(ldtype=='wload'):
                            ltype=1                                
                            ldtuple=(ltype,i+1,k+1,wa,wb,a,kref)
                        cls.memloads.append(ldtuple)                        
            i +=1    
            load=load.GetNext()
        cls.nlc=i    
    
    @classmethod
    def XML_reader(cls,filein):
    # start processing the XML file
        fileout = open("output.txt", "w")
        doc = wx.xml.XmlDocument()
        if not doc.Load(filein):
            return False
        cls.strutype = str(doc.GetRoot().GetName()).ljust(10)

        StruMod.ndf=cls.structure(cls.strutype)
        fileout.write('{0} Degrees of Freedom per node: {1}\n'.format(cls.strutype,StruMod.ndf))
        child = doc.GetRoot().GetChildren()
        while child:
            tagname = child.GetName()
            content = child.GetNodeContent()  # process text enclosed by tag1/tag1
            if tagname == "title":
                cls.exampletitle=str(content).ljust(80)
                fileout.write('Project Name: {0}\n'.format(cls.exampletitle))
            elif tagname == "code":
                (code, StruMod.fyield)=cls.code(content,child)
                fileout.write('Code {0} Fy= {1:.2f}\n'.format(code,StruMod.fyield))
            elif tagname == "material":
                (cls.matlist,  cls.materials)=cls.material(content,child)
                #fileout.write('{0}\n {1}\n'.format("Material",StruMod.mat_table))
                fileout.write('{0}\t\t{1}\n'.format('Number of Materials:', cls.nmat).expandtabs(10))
            elif tagname == "section":
                #(cls.seclist,cls.sections)=cls.section(content,child)
                cls.section(content, child)
                #fileout.write('{0}\n {1}\n'.format("Sections",cls.sections_arr))
                fileout.write('{0}\t{1}\n'.format('Number of Sections:',cls.nsec).expandtabs(10))
            elif tagname == "nodes":
                (StruMod.nn,StruMod.n,StruMod.coor,StruMod.nodelist)=cls.nodes(content,child)
                #fileout.write('Number of Nodes: {0}\nNumber of Equations: {1}\n'.format(cls.nn,StruMod.n))
                fileout.write('{0}\t{1}\n'.format('Number of Nodes:',cls.nn).expandtabs(10))
            elif tagname == "elements": 
                cls.elem_prop(content,child)
                #fileout.write('Number of Elements: {0}\n Bandwidth: {1}\n{2}\n'.format(cls.ne,cls.ms,cls.elem_prop_arr))
                fileout.write('{0}\t{1}\n'.format('Number of Elements:',cls.ne).expandtabs(10))
            elif tagname == "boundary":
                cls.boundary(content,child)
                cls.ib=np.reshape(cls.boundaries,newshape=((cls.ndf+1)*cls.nbn))
                #fileout.write('Number of Boundaries: {0}\n {1}\n'.format(cls.nbn,cls.boundaries))
                fileout.write('{0}\t{1}\n'.format('Number of Boundaries:',cls.nbn).expandtabs(10))
                #print(cls.ib)
            elif tagname == "loading": 
                cls.loading(content,child)
                #fileout.write('Number of Loading Cases: {0}\n {1}\n {2}\n'.format(cls.nlc,cls.nodeloads,cls.memloads))
                fileout.write('Number of Loading Cases: {0}\n Loaded Nodes: {1}\n Loaded Members: {2}\n'.format(
                    cls.nlc, cls.nlnodes, cls.nlmem))
                cls.al=np.zeros((cls.n,cls.nlc))
                cls.reac=np.zeros((cls.n,cls.nlc))
                cls.mfem_param=np.reshape(cls.memloads,(cls.nlmem,7))
                #print(cls.mfem_param)

                for nload in cls.nodeloads:
                    n1=nload[1]
                    klc=nload[0]
                    kdsp=cls.ndf*(n1)
                    cls.al[kdsp][klc] = nload[2]
                    cls.al[kdsp+1][klc] = nload[3]
                    cls.al[kdsp+2][klc] = nload[4]
                #print(cls.al)    
            child = child.GetNext()
        fileout.close()        
