from tkinter import Canvas, Frame, BOTH


class Tkwireframe2D(Frame):

    def __init__(self, root, x, y, incidence, ne):
        super().__init__()
        self.root = root
        self.x = x
        self.y = y
        self.incidence = incidence
        self.ne = ne

        #self.initUI()
        self.drawLines()

    def initUI(self):

        self.root.title("Lines")
        self.pack(fill=BOTH, expand=1)

        canvas = Canvas(self)
        #This creates a straight horizontal line of length 200
        canvas.create_line(15, 25, 200, 25)
        #This creates a vertical dashed line of length 300
        canvas.create_line(300, 35, 300, 200, dash=(4, 2))
        #This creates a triangle
        #canvas.create_line(55, 85, 155, 85, 105, 180, 55, 85)
        #This packs the canvas to the main window and makes
        canvas.pack(fill=BOTH, expand=1)

    def drawLines(self):
        self.root.title("Lines2d")
        self.pack(fill=BOTH, expand=1)
        canvas = Canvas(self.root, width=300, height=200)
        canvas.pack()
        #print('2DLines')
        canvas.create_line(300, 35, 300, 200, dash=(4, 2))
        i = 0
        while i < self.ne:
            canvas.create_line(self.x[self.incidence[i][0]], self.y[self.incidence[i][0]],
                self.x[self.incidence[i][1]], self.y[self.incidence[i][1]])
            i += 1
        canvas.pack(fill=BOTH, expand=1)
