import wx.xml
import sqlite3
import math
import numpy as np
#import scipy
#import wx.dataview

g=9.806
ndf=0
lineinput = []
fyield=0.0
scaleS = 0.0
scaleden=0.0
emod=''
matden=''
poisson=''
materials = []
sections=[]
x=[]
y=[]
z=[]
nodes=[]
coor=[]
nodelist=[]
seclist=[]
matlist=[]
elemlist=[]
elements=[]
steel = ('', 'Steel', 200e+06, 78.5, 0.28)
titanium = ('', 'Titaniun', 113e+06, 44.13, 0.3)
def TokNperM2(unitF):
    if unitF == "kN/m2":
        tonewton=1.0
    elif unitF =="lb/sqf":
        tonewton = 0.047880258888889
    elif unitF=="ksi":
        tonewton = 6894.757280000001
    elif unitF=="kgf/m2":
        tonewton=g
    elif unitF=="ton/sqf":
        tonewton = 107.25177991111
    elif unitF=="psi":
        tonewton = 6.89475728
    else:
        tonewton=1.0
    return tonewton        
        
def TokNperM3(unitF):
    if unitF=="kN/m3":
        tonewton=1.0
    elif unitF=="lbf/ft3":
        tonewton = 0.1570865731
    elif unitF=="kips/inch3":
        tonewton = 271447.14116097
    elif unitF=="kgf/m3":
        tonewton=g/1000.0
    elif unitF=="tonf/ft3":
        tonewton = 314.1731461238
    elif unitF=="t/m3":
        tonewton=g
    else:
        tonewton=1.0
    return tonewton            

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
def XML_reader(filein):
    # start processing the XML file
    doc = wx.xml.XmlDocument()
    if not doc.Load(filein):
        return False
    global ndf
    strutype = doc.GetRoot().GetName()
    if strutype =="Frame2D":   ndf=3
    elif strutype =="Frame3D": ndf=6
    elif strutype =="Truss3D": ndf=3
    elif strutype =="Truss2D": ndf=2
    elif strutype =="Grid":    ndf=3
    elif strutype =="Frame2D_8DOF": ndf=4
    else: ndf=3    
    #     if child.GetType() == wx.xml.XML_PI_NODE and child.GetName() == "target":
            # Other code here...
    child = doc.GetRoot().GetChildren()
    while child:
        tagname = child.GetName()
        content = child.GetNodeContent()  # process text enclosed by tag1/tag1
        if tagname == "title":
            projName=content
        elif tagname == "code":
            UnitS = child.GetAttribute("unitS", "kN/m2")  # UnitS: stress unit ...
            #print(UnitS)
            if UnitS == "default-value": scaleS=1.0
            else: scaleS=TokNperM2(UnitS)                    
            content=content.replace("="," ")
            content=content.replace("\n"," ")
            lineinput=content.split()
            #print(lineinput)
            #print(scaleS)
            global fyield
            i=0
            while i<len(lineinput):
                if lineinput[i]=="code":
                    Code=lineinput[i+1]
                elif lineinput[i]=="fy":
                    fyield = float(lineinput[i+1])*scaleS
                i +=1    
            #print(fyield)
            lineinput.clear()
        elif tagname == "material":
            UnitS = child.GetAttribute("unitS", "kN/m2")
            scaleden=child.GetAttribute("den","kN/m3")
            if UnitS == "default-value":
                scaleS = 1.0
            else:
                scaleS = TokNperM2(UnitS)
            if scaleden=="default-value":
                toknm2=1.0
            else:
                toknm3=TokNperM3(scaleden)
            lines=content.splitlines()     
            #content = content.replace("=", " ")
            #content = content.replace("\n", " ")
            global materials
            for line in lines:
                line = line.replace("=", " ")
                lineinput = line.split()
                i=0
                #print(lineinput)
                matlist.append(lineinput[0])
                matype=lineinput[1]
                if matype=='General':
                    while i<len(lineinput):
                        if lineinput[i]=="E":
                            global emod
                            emod=float(lineinput[i+1])*scaleS
                        elif lineinput[i]=="Density":
                            global matden
                            matden=float(lineinput[i+1])*toknm3
                        elif lineinput[i]=="Poisson":
                            global poisson
                            poisson=float(lineinput[i+1])
                        i +=1
                    material=(matype,emod,matden,poisson) 
                    materials.append(material)
                elif matype=='Steel':
                    y=list(steel)
                    y[0] = matid
                    material=tuple(y)
                    materials.append(material)
                elif matype=='Titanium':
                    y = list(titanium)
                    y[0] = matid
                    material = tuple(y)
                    materials.append(material)
        elif tagname == "section":
            UnitL = child.GetAttribute("unitL", "m")
            if UnitL == "default-value":
                scaleL = 1.0
            else:
                scaleL = ToMeter(UnitL)
            lines=content.splitlines()
            global sections
            for line in lines:
                line = line.replace("=", " ")
                lineinput = line.split()
                i = 0
                #print(lineinput)
                seclist.append(lineinput[0])
                sectype=lineinput[1]
                if sectype=='Tube':
                    while i<len(lineinput):
                        if lineinput[i]=='OD':
                            od=float(lineinput[i+1])*scaleL
                        elif lineinput[i]=='WTH':
                            wth=float(lineinput[i+1])*scaleL
                        i +=1
                    section=pipeparam(od,wth)
                    #y=list(section)
                    #y[0]=secid
                    #section=tuple(y)
                    sections.append(section)                    
                elif sectype=='EDI':
                    edi= lineinput[2]
                    # Connecto to database
                    conn=sqlite3.connect('/home/ev/pyLSA/aisc_shapes_v15_US_R1.db')
                    # Create cursor
                    c=conn.cursor()
                    # Query the database
                    c.execute("SELECT * FROM shapesv15_US WHERE EDI=edi")
                    items=c.fetchall()
                    for item in items:
                        if item[0]==edi:
                            y=list(item)
                            y[1]=y[1]*0.01459 # w
                            y[2]= y[2]*scaleL**2 # A
                            y[3]= y[3]*scaleL # d
                            y[4]=y[4]*scaleL**4 # Ix
                            y[5] = y[5]*scaleL**3  # Zx
                            y[6] = y[6]*scaleL**3  # Sx
                            y[7] = y[7]*scaleL  # rx
                            y[8] = y[8]*scaleL**4  # Iy
                            y[9] = y[9]*scaleL**3  # Zy
                            y[10] = y[10]*scaleL**3  # Sy
                            y[11] = y[11]*scaleL  # ry
                            y[12] = y[12]*scaleL**4  # J
                            item=tuple(y)
                            sections.append(item)
                            #print(item)
        elif tagname == "nodes":
            UnitL = child.GetAttribute("unitL", "m")
            if UnitL == "default-value":
                scaleL = 1.0
            else:
                scaleL = ToMeter(UnitL)
            lines = content.splitlines()
            for line in lines:
                #line = line.replace("=", " ")
                lineinput = line.split()
                nodelist.append(lineinput[0])
                nodex=float(lineinput[1])
                nodey=float(lineinput[2])
                if len(lineinput) >3:
                    nodez=float(lineinput[3])
                else: 
                    nodez=0.0
                nodes = (nodex, nodey, nodez)
                coor.append(nodes)    
        elif tagname == "elements":
            lines = content.splitlines()
            for line in lines:
                lineinput = line.split()
                elemlist.append(lineinput[0])
                inc1=nodelist.index(lineinput[1])
                inc2=nodelist.index(lineinput[2])
                secid=seclist.index(lineinput[3])
                matid=matlist.index(lineinput[4])
                pt1=np.array(coor[inc1])
                pt2=np.array(coor[inc2])
                temp=pt1-pt2
                elemlen=np.linalg.norm(temp)
                axis_x=np.array((1,0,0))
                cosx=np.dot(temp,axis_x.T)/elemlen
                element=(inc1,inc2,secid,matid,elemlen,cosx)
                elements.append(element)
        elif tagname == "boundary":
            lines = content.splitlines()
        elif tagname == "loading":
            UnitL = child.GetAttribute("unitL", "m")
            if UnitL == "default-value":
                scaleL = 1.0
            else:
                scaleL = ToMeter(UnitL)
            lines = content.splitlines()
                
        child = child.GetNext()
