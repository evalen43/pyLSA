import wx.xml
import sqlite3
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
    #print(strutype)

    #     if child.GetType() == wx.xml.XML_PI_NODE and child.GetName() == "target":

    #         # process Process Instruction contents
    #         pi = child.GetContent()

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
                matid=lineinput[0]
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
                    material=(matid,matype,emod,matden,poisson) 
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
            secid = lineinput[0]
            sectype=lineinput[1]
            if sectype=='Tube':
                while i<len(lineinput):
                    if lineinput[i]=='OD':
                        od=lineinput[i+1]
                    elif lineinput[i]=='WTH':
                        wth=lineinput[i+1]        
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

                
        child = child.GetNext()
