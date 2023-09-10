using System;
using static System.Console;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static Color;

enum Color {Red ,Black}

namespace RedBlackTreem
{
   
    class Node
    {

        public int value;
        public Color colour;
        public Node left;
        public Node right;
        public Node parent;
        public static Node Null = new Node(Black);
        public Node(int value, Color colour, Node left, Node right, Node parent)
        {
            this.value = value;
            this.colour = colour;
            this.left = left;
            this.right = right;
            this.parent = parent;

        }
        public Node(int value, Node left, Node right, Node parent)
        {
            this.value = value;
            this.left = left;
            this.right = right;
            this.parent = parent;

        }
        public Node(Color colour)
        {
            this.colour = colour;
        }
    }
    class RedBlackTree
    {
        public Node root;
        public RedBlackTree()
        {
            root = Node.Null;
            root.colour = Color.Black;
        }
        
        public  bool Search(int valueToBeSearched, Node x)
        {
            if (x == Node.Null)
            {

                return false;

            }

            else if (x.value == valueToBeSearched)
            {
                return true;
            }

            else if (x.value < valueToBeSearched)
            {
                return Search(valueToBeSearched, x.right);
            }

            else
            {
                return Search(valueToBeSearched, x.left);
            }

        }

        public int Minimum()
        {
            if (root == Node.Null)
            {
                WriteLine("The minimum can not be found because the tree is empty.");
            }

            Node minNode = root;

            while (minNode.left != Node.Null)
            {
                minNode = minNode.left;
            }

            return minNode.value;
        }

        public int Maximum()
        {
            if (root == Node.Null)
            {
                throw new InvalidOperationException("Tree is empty.");
            }

            Node maxNode = root;

            while (maxNode.right! != Node.Null)
            {
                maxNode = maxNode.right;
            }

            return maxNode.value;

        }

        public Node Successor(Node targetSucc)
        {
            if (targetSucc == Node.Null)
            {
                WriteLine("Target node is null.");
            }

            if (targetSucc.right != Node.Null)
            {
                targetSucc = targetSucc.right;

                while (targetSucc.left != Node.Null)
                {
                    targetSucc = targetSucc.left;
                }
                return targetSucc;
            }

            else
            {
                Node parent = targetSucc.parent;

                while (parent != Node.Null && targetSucc == parent.right)
                {
                    targetSucc = parent;
                    parent = parent.parent;
                }
                return parent;
            }
        }

        public Node Predecessor(Node targetPred)
        {
            if (targetPred == Node.Null)
            {
                WriteLine("Target node is null.");
            }

            if (targetPred.left != Node.Null)
            {
                targetPred = targetPred.left;

                while (targetPred.right != Node.Null)
                {
                    targetPred = targetPred.right;
                }
                return targetPred;
            }

            else
            {
                Node parent = targetPred.parent;

                while (parent != Node.Null && targetPred == parent.left)
                {
                    targetPred = parent;
                    parent = parent.parent;
                }

                return parent;
            }
        }

        private  void LeftRotate(Node x)
        {
            var y = x.right;
            x.right = y.left;
            if (y.left != Node.Null)
            {
                y.left.parent = x;
            }

            y.parent = x.parent;

            if (x.parent == Node.Null)
            {
                root = y;
            }

            else if (x == x.parent.left)
            {
                x.parent.left = y;
            }

            else
            {
                x.parent.right = y;
            }

            y.left = x;
            x.parent = y;
        }
        private  void RightRotate(Node x)
        {
            var y = x.left;
            x.left = y.right;

            if (y.right != Node.Null)
            {
                y.right.parent = x;
            }

            y.parent = x.parent;

            if (x.parent == Node.Null)
            {
                root = y;
            }

            else if (x == x.parent.right)
            {


                x.parent.right = y;
            }

            else
            {
                x.parent.left = y;
            }

            y.right = x;
            x.parent = y;
        }
        private  void InsertFix (Node z)
        {

            while (z.parent.colour == Red)
            {
                if (z.parent == z.parent.parent.left)
                {
                    var y = z.parent.parent.right;
                    if (y.colour == Red)
                    {
                        z.parent.colour = Black;
                        y.colour = Black;
                        z.parent.parent.colour = Red;
                        z = z.parent.parent;
                    }
                    else
                    {
                        if (z == z.parent.right)
                        {
                            z = z.parent;
                            this.LeftRotate(z);


                        }
                        z.parent.colour = Black;
                        z.parent.parent.colour = Red;
                        this.RightRotate( z.parent.parent);
                    }
                }

                else
                {
                    var y = z.parent.parent.left;
                    if (y.colour == Red)
                    {
                        z.parent.colour = Black;
                        y.colour = Black;
                        z.parent.parent.colour = Red;
                        z = z.parent.parent;
                    }

                    else
                    {
                        if (z == z.parent.left)
                        {
                            z = z.parent;
                            this.RightRotate(z);


                        }
                        z.parent.colour = Black;
                        z.parent.parent.colour = Red;
                        this.LeftRotate(z.parent.parent);
                    }
                }
            }
            root.colour = Black;
        }
        public bool Insert(int s)
        {
            if (Search(s, root))
            {
                return false;
            }
            else
            {
                Node z = new Node(s, Node.Null, Node.Null, Node.Null);

                var y = Node.Null;
                var x = root;
                while (x != Node.Null)
                {
                    y = x;
                    if (z.value < x.value)
                    {
                        x = x.left;

                    }
                    else
                    {
                        x = x.right;
                    }
                }
                z.parent = y;
                if (y == Node.Null)
                {
                    root = z;

                }
                else
                {
                    if (z.value < y.value)
                    {
                        y.left = z;
                    }
                    else
                    {
                        y.right = z;
                    }
                }

                z.left = Node.Null;
                z.right = Node.Null;
                z.colour = Red;
                this.InsertFix(z);
                return true;
            }
        }
        private static Node? SuitableReplacemetInsert(int val, Node N)
        {
            if (N == null)
            {

                return null;

            }

            else if (N.value == val)
            {
                return N;
            }
            else if (N.value < val)
            {
                return SuitableReplacemetInsert(val, N.right);
            }
            else
            {
                return SuitableReplacemetInsert(val, N.left);
            }

        }
        private static Node DeleteReplacemet(Node t)
        {

            Node temp = t.right;
            while (temp.left != Node.Null)
            {

                temp = temp.left;
            }
            return temp;
        }
        public  void DeleteFix(Node x)
        {
            while ((x != root) && (x.colour == Black))
            {
                if (x == x.parent.left)
                {
                    var w = x.parent.right;
                    if (w.colour == Red)
                    {
                        w.colour = Black;
                        x.parent.colour = Red;
                        this.LeftRotate( x.parent);
                        w = x.parent.right;
                    }
                    if ((w.left.colour == Black) && (w.right.colour == Black))
                    {
                        w.colour = Red;
                        x = x.parent;

                    }
                    else
                    {
                        if (w.right.colour == Black)
                        {
                            w.left.colour = Black;
                            w.colour = Red;
                            this.RightRotate(w);
                            w = x.parent.right;
                        }
                        w.colour = x.parent.colour;
                        x.parent.colour = Black;
                        w.right.colour = Black;
                        this.LeftRotate(x.parent);
                        x = root;

                    }
                }
                else
                {
                    var w = x.parent.left;
                    if (w.colour == Red)
                    {
                        w.colour = Black;
                        x.parent.colour = Red;
                        this.RightRotate(x.parent);
                        w = x.parent.left;
                    }
                    if ((w.right.colour == Black) && (w.left.colour == Black))
                    {
                        w.colour = Red;
                        x = x.parent;

                    }
                    else
                    {
                        if (w.left.colour == Black)
                        {
                            w.right.colour = Black;
                            w.colour = Red;
                            this.LeftRotate(w);
                            w = x.parent.left;
                        }
                        w.colour = x.parent.colour;
                        x.parent.colour = Black;
                        w.left.colour = Black;
                        this.RightRotate(x.parent);
                        x = root;

                    }
                }
            }
            x.colour = Black;
        }
        public  bool Delete(int f)
        {
            Node x;
            Node y;
            if (Search(f, root) == false)
            {
                return false;
            }
            else
            {
                var z = SuitableReplacemetInsert(f, root);

                if (z.left == Node.Null || z.right == Node.Null)
                {
                    y = z;
                }
                else
                {
                    y = DeleteReplacemet(z);

                }
                if (y.left != Node.Null)
                {
                    x = y.left;
                }
                else
                {
                    x = y.right;
                }

                if (x != null)
                {
                    x.parent = y.parent;
                }

                if (y.parent == Node.Null)
                {
                    root = x;
                }
                else
                {
                    if (y == y.parent.left)
                    {
                        y.parent.left = x;
                    }
                    else
                    {
                        y.parent.right = x;
                    }
                }
                if (y != z)
                {
                    z.value = y.value;
                }
                if (y.colour == Black)
                {
                    this.DeleteFix(x);
                }
            }
            return true;
        }

        private int BlackHeight(Node n, int counter)
        {
            if (n == null)
            {
                return 1;
            }
            if (n.colour == Black)
            {
                counter++;

            }
            if ((n.left == null) && (n.right == null))
            {
                return counter;
            }
            else if ((n.left != null) && (n.right == null))
            {
                if (counter == BlackHeight(n.left, counter))
                {
                    return counter;
                }
                else
                {

                    return -1;


                }
            }
            else if ((n.left == null) && (n.right != null))
            {
                if (counter == BlackHeight(n.right, counter))
                {
                    return counter;
                }
                else
                {

                    return -1;

                }
            }
            else if ((n.left != null) && (n.right != null))
            {
                if (BlackHeight(n.left, counter) != BlackHeight(n.right, counter))
                {

                    return -1;
                }
                else
                {
                    return BlackHeight(n.left, counter);

                }
            }

            else
            {

                return -1;
            }

        }
        private  bool RedCondition(Node n)
        {
            if (n == Node.Null)
            {
                return true;
            }
            if (n.colour == Red)
            {
                if ((n.left.colour == Red) || (n.right.colour == Red))
                {
                    return false;
                }

            }
            if (n.left != Node.Null)
            {
                return RedCondition(n.left);
            }
            if (n.right != Node.Null)
            {
                return RedCondition(n.right);
            }
            return true;
        }
        private  bool BlackRoot(RedBlackTree T)
        {
            if (T.root.colour == Black)
            {
                return true;
            }
            else
            {
                return false;
            }
        }
        public  bool Check()
        {
            if ((BlackHeight(root, 0) != -1) && (RedCondition(root) != false) && (BlackRoot(this) != false))
            {
                return true;
            }
            else return false;
        }
        public static void Print(Node p, int padding)
        {
            if (p != Node.Null)
            {
                if (p.right != Node.Null)
                {
                    Print(p.right, padding + 4);
                }
                if (padding > 0)
                {
                    Console.Write(" ".PadLeft(padding));
                }
                if (p.right != Node.Null)
                {
                    Console.Write("/\n");
                    Console.Write(" ".PadLeft(padding));
                }
                Console.Write($"{p.value} [{p.colour}]\n");
                if (p.left != Node.Null)
                {
                    Console.Write(" ".PadLeft(padding) + "\\\n");
                    Print(p.left, padding + 4);
                }
            }
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            RedBlackTree T = new RedBlackTree();
            Random rand = new Random();

            for (int i = 1; i <= 10; i++)
            {
                int n = rand.Next(1, 100);
                T.Insert(n);
            }

            int minNode = T.Minimum();
            WriteLine("The minimum value in the tree: " + minNode);

            int maxNode = T.Maximum();
            WriteLine("The maximum value in the tree: " + maxNode);

            Node targetSucc = T.root;
            Node successor = T.Successor(targetSucc);
            Node predecessor = T.Predecessor(targetSucc);

            WriteLine("Successor of the node: " + (successor != null ? successor.value.ToString() : "No successor."));
            WriteLine("Predecessor of the node: " + (predecessor != null ? predecessor.value.ToString() : "No predecessor."));

            WriteLine("Tree structure: ");
            RedBlackTree.Print(T.root, 0);

            if (T.Check())
            {
                Console.WriteLine("The tree satisfies Red-Black Tree properties.");
            }
            else
            {
                Console.WriteLine("The tree does not satisfy Red-Black Tree properties.");
            }
        }
    }
}