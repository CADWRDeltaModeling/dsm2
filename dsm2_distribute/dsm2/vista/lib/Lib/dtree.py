from java.awt.dnd import *
from java.awt.datatransfer import *
from java.awt import Point
from java.io import *
from javax.swing.tree import *
from javax.swing import JTree

class DragableTree(JTree):
    def __init__(self,value=None):
        if value != None:
            JTree.__init__(self,value)
        else:
            JTree.__init__(self)
        self.initialize()
    def initialize(self):
        self.gestureListener = TreeGestureListener(self)
        self.sourceListener = TreeSourceListener(self)
        self.targetListener = TreeTargetListener(self)
        # Set up the tree to be a drop target
        self.dropTarget = DropTarget(self,DnDConstants.ACTION_COPY_OR_MOVE,self.targetListener,1)
        # Set up the tree to be a drag source
        self.dragSource = DragSource.getDefaultDragSource()
        self.dragSource.createDefaultDragGestureRecognizer(self,DnDConstants.ACTION_COPY_OR_MOVE,self.gestureListener)
    def getTreeNode(self,location):
        treePath = self.getPathForLocation(location.x,location.y)
        row = self.getRowForLocation(location.x,location.y)
        if treePath != None:
            return treePath.getLastPathComponent()
        return None

class TreeGestureListener(DragGestureListener):
    def __init__(self,tree):
        self.tree = tree
    def dragGestureRecognized(self,evt):
        tree = self.tree
        nodeLocation = evt.getDragOrigin()
        treeNode = tree.getTreeNode(nodeLocation)
        if treeNode != None and isinstance(treeNode,DefaultMutableTreeNode):
            tree.dragNode = treeNode
            #print tree.dragNode.toString()
            tree.dragSource.startDrag(evt,DragSource.DefaultCopyDrop,StringSelection(treeNode.toString()),tree.sourceListener)
        return

class TreeSourceListener(DragSourceListener):
    def __init__(self,tree):
        self.tree = tree
    def dragDropEnd(self,evt):
        return
    def dragEnter(self,evt):
        return
    def dragExit(self,evt):
        return
    def dragOver(self,evt):
        return
    def dropActionChanged(self,evt):
        return

class TreeTargetListener(DropTargetListener):
    def __init__(self,tree):
        self.tree = tree
    def drop(self,evt):
        tree = self.tree
        try:
            dropLocation = evt.getLocation()
            treeNode = tree.getTreeNode(dropLocation)
            stringFlavor = DataFlavor.stringFlavor
            data = evt.getTransferable()
            if treeNode != None and evt.isDataFlavorSupported(stringFlavor):
                text = data.getTransferData(stringFlavor)
                evt.acceptDrop(DnDConstants.ACTION_COPY_OR_MOVE)
                dragParent=None
                if not(isinstance(treeNode,DragableTreeNode)) or treeNode.allowsNodeType(tree.dragNode):
                    dragParent = tree.dragNode.getParent()
                    tree.getModel().removeNodeFromParent(tree.dragNode)
                    treeNode.add(tree.dragNode)
                    tree.getModel().reload(treeNode)
                    #print 'Tree Node: %s' % treeNode.toString()
                    evt.dropComplete(1)
                else:
                    evt.dropComplete(0)
                tree.getModel().nodeStructureChanged(treeNode.getParent())
                tree.setSelectionPath(TreePath(treeNode.getPath()))
                tree.expandPath(TreePath(treeNode.getPath()))
                if dragParent != None:
                    tree.expandPath(TreePath(dragParent.getPath()))
            else:
                evt.rejectDrop()
        except IOException:
            print "DragableTree drop error (IOException)"
        except UnsupportedFlavorException:
            print "DragableTree drop error (UnsupportedFlavorException)"
        return
    def dragEnter(self,evt):
        if self.isDragOk(evt):
            evt.acceptDrag(DnDConstants.ACTION_COPY_OR_MOVE)
        else:
            evt.rejectDrag()
        return
    def dragExit(self,evt):
        return
    def dragOver(self,evt):
        tree = self.tree
        dragLocation = evt.getLocation()
        treePath = tree.getPathForLocation(dragLocation.x,dragLocation.y)
        if treePath != None and self.isDragOk(evt):
            # Make the node active
            tree.setSelectionPath(treePath)
            evt.acceptDrag(DnDConstants.ACTION_COPY_OR_MOVE)
        else:
            evt.rejectDrag()
        return
    def dropActionChanged(self,evt):
        if self.isDragOk(evt):
            evt.acceptDrag(DnDConstants.ACTION_COPY_OR_MOVE)
        else:
            evt.rejectDrag()
        return
    def isDragOk(self,evt):
        return 1

class DragableTreeNode(DefaultMutableTreeNode):
    """
    A tree node to retain a user object. This was needed as the attributes
    of python objects are invisible to java classes.
    """
    def __init__(self,obj,name,folder=0):
        DefaultMutableTreeNode.__init__(self,name,folder)
        self.obj = obj
    def getUserObject(self):
	return self.obj
    def setUserObject(self,obj):
	if type(obj) == type(''):
	    DefaultMutableTreeNode.setUserObject(self,obj)
	else:
	    self.obj = obj
    def allowsNodeType(self,node):
        if isinstance(node,DragableTreeNode): return 1
        return 0

class VPlotterNode(DragableTreeNode):
    def __init__(self,obj,name,folder=0):
        DragableTreeNode.__init__(self,obj,name,folder)
    def executeAction():
        return

class StudyNode(VPlotterNode):
    def __init__(self,obj,name,folder=1):
        VPlotterNode.__init__(self,obj,name,folder)
    def allowsNodeType(self,node):
        if isinstance(node,GraphNode): return 1
        return 0

class GraphNode(VPlotterNode):
    def __init__(self,obj,name,folder=1):
        VPlotterNode.__init__(self,obj,name,folder)
    def allowsNodeType(self,node):
        if isinstance(node,PlotNode): return 1
        return 0

class PlotNode(VPlotterNode):
    def __init__(self,obj,name,folder=0):
        VPlotterNode.__init__(self,obj,name,folder)
    def allowsNodeType(self,node):
        return 0
