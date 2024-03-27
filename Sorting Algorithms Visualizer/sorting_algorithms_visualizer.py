import tkinter as tk
from tkinter import ttk
import random
import time

number_of_indices = 10

def bubble_sort():
    """
    Creates an array a by picking random numbers and calls the visualizing function to sort them using bubble sort
    """
    a = [random.randint(0, 500) for _ in range(number_of_indices)]       
    bubble_sort_visualize(a)

def bubble_sort_visualize(a):
    """
    Main sorting function for bubble sort that updates the elements after every iteration
    """
    n = len(a)
    for i in range(n - 1, 0, -1):
        for j in range(i):
            if a[j] > a[j + 1]:
                a[j], a[j + 1] = a[j + 1], a[j]
                draw_bubble(a, j, j + 1)   # drawing the swapping event
                root.update()
                time.sleep(1.0)  # using time.sleep to make the visualization a little slower for the ease of the user
    draw_bubble(a, None, None)  # drawing the array again with parameters a, None, None to show the user the sorted array with indices with initial colors
    root.update()

def draw_bubble(a, curr, sw):   # takes parameters the array, current element and the element to be swapped with
    """
    Function to draw the bubble sort on the canvas
    """
    canvas.delete("all")    # clearing the canvas before drawing
    left_margin = 15
    width = 50
    height = 50
    padding = 10

    for i, el in enumerate(a):  
        if i == curr:
            color = "steel blue"
        elif i == sw:
            color = "red4"
        else:
            color = "gray6"

        canvas.create_rectangle(left_margin, 100, left_margin + width, 100 + height, fill=color)       
        canvas.create_text(left_margin + width // 2, 100 + height // 2, text=str(el), fill="snow") 
        left_margin += width + padding # incrementing left margin to draw the next square in place

def selection_sort():
    """
    Creates an array a by picking random numbers and calls the visualizing function to sort them using selection sort
    """
    a = [random.randint(0, 500) for _ in range(number_of_indices)]
    selection_sort_visualize(a)

def selection_sort_visualize(a):
    """
    Main selection sort function that updates the elements each iteration
    """
    n = len(a)
    for i in range(n - 1):
        min_index = i
        min_val = a[i]
        for j in range(i + 1, n):
            if a[j] < min_val:
                min_val = a[j]
                min_index = j
        a[i], a[min_index] = a[min_index], a[i]
        draw_selection(a, i, min_index)     # drawing the swap by calling the draw_selection function
        root.update()
        time.sleep(1.0)
    draw_selection(a, None, None)   # when the sorting loop is over we call the draw_selection function again to see the last version of the array
    root.update()

def draw_selection(a, curr, min_index):     # takes parameters array, the current element and the minimum index 
    """
    Function to draw the selection sort on the canvas
    """
    canvas2.delete("all")
    left_margin = 15
    width = 50
    height = 50
    padding = 10

    for i, el in enumerate(a):  
        if i == curr:
            color = "steel blue"
        elif i == min_index:
            color = "red4"
        else:
            color = "gray6"

        canvas2.create_rectangle(left_margin, 100, left_margin + width, 100 + height, fill=color)
        canvas2.create_text(left_margin + width // 2, 100 + height // 2, text=str(el), fill="snow")
        left_margin += width + padding


def insertion_sort():
    """
    Creates an array a by picking random numbers and calls the visualizing function to sort them using insertion sort
    """
    a = [random.randint(0, 500) for _ in range(number_of_indices)]
    insertion_sort_visualize(a)

def insertion_sort_visualize(a):
    """
    Main insertion sort function that updates the elements while also letting the user know which element is "lifted up" 
    to create an empty space for others to be shifted each iteration
    """
    n = len(a)
    for i in range(n):
        t = a[i]    # saving the element to be lifted up in a temporary variable t

        canvas3.create_text(300, 220, text=f"Lifting up [{t}]", font=('Saab', 15, 'bold'))
        root.update()
        time.sleep(1.0)

        j = i - 1
        
        while j >= 0 and a[j] > t:
            a[j + 1] = a[j]
            j -= 1
            draw_insertion(a, j + 1, j + 2)
            root.update()
            time.sleep(1.0)
        a[j + 1] = t
        draw_insertion(a, j, j + 1)
        root.update()
        time.sleep(1.0)
    draw_insertion(a, None, None)
    root.update()

def draw_insertion(a, curr, j):
    """
    Function to draw the insertion sort on the canvas
    """
    canvas3.delete("all")
    left_margin = 15
    width = 50
    height = 50
    padding = 10

    for i, el in enumerate(a):
        if i == curr:
            color = "steel blue"
        elif i == j:
            color = "red4"
        else:
            color = "gray6"

        canvas3.create_rectangle(left_margin, 100, left_margin + width, 100 + height, fill=color)
        canvas3.create_text(left_margin + width // 2, 100 + height // 2, text=str(el), fill="snow")
        left_margin += width + padding

def merge_sort():
    """
    Creates an array a by picking random numbers and calls the visualizing function to sort them using merge sort
    """
    a = [random.randint(0, 500) for _ in range(number_of_indices)]
    merge_sort_visualize(a)

def merge_sort_visualize(a):
    """
    Main merge sort function that sorts the array 
    """
    draw_merge(a, None, None)

    if len(a) > 1:
        mid = len(a) // 2
        left = a[:mid]
        right = a[mid:]
        
        canvas4.create_text(300, 220, text=f'Splitting the array {a}\ninto {left} and {right}.', font=('Saab', 15, 'bold'))
        root.update()
        time.sleep(1.0)

        merge_sort_visualize(left)
        merge_sort_visualize(right)

        i = j = k = 0

        while i < len(left) and j < len(right):
            if left[i] < right[j]:
                a[k] = left[i]
                i += 1
            else:
                a[k] = right[j]
                j += 1
            k += 1

        while i < len(left):
            a[k] = left[i]
            i += 1
            k += 1

        while j < len(right):
            a[k] = right[j]
            j += 1
            k += 1

        canvas4.create_text(300, 220, text=f"Sorting and then\nmerging the parts {left} and {right}.", font=('Saab', 15, 'bold'))
        root.update()
        time.sleep(1.0)


        draw_merge(a, left, right)
        root.update()
        time.sleep(1.0)

    draw_merge(a, None, None)
    root.update()

def draw_merge(a, left, right):
    """
    Function to draw the merge sort on the canvas taking the left and right subarrays
    """
    canvas4.delete("all")
    left_margin = 15
    width = 50
    height = 50
    padding = 10

    if left is None and right is None:
        color = "gray6"

    for i, el in enumerate(a):
        if left and el in left:
            color = "red4"
        elif right and el in right:
            color = "chartreuse4"
        else:
            color = "gray6"

        canvas4.create_rectangle(left_margin, 100, left_margin + width, 100 + height, fill=color)
        canvas4.create_text(left_margin + width // 2, 100 + height // 2, text=str(el), fill="snow")
        left_margin += width + padding


def quick_sort():
    """
    Creates an array a by picking random numbers and calls the visualizing function to sort them using quick sort and
    calls draw_quick with parameters (a, None, None, None) to show the user the last version of the array with initial colors
    """
    a = [random.randint(0, 500) for _ in range(number_of_indices)]
    quick_sort_visualize(a, 0, len(a) - 1)
    draw_quick(a, None, None, None)

def quick_sort_visualize(a, lo, hi):
    """
    Main quicksort function that recursively calls itself to sort its right and left partitions 
    """
    if lo < hi:
        piv = partition(a, lo, hi)
        quick_sort_visualize(a, lo, piv)
        quick_sort_visualize(a, piv + 1, hi)

def partition(a, lo, hi):    
    """
    Helper function for quick sort to partition the array using Hoare partitioning method
    """
    piv = a[lo]             
    i, j = lo - 1, hi + 1

    while True:
        i += 1
        while a[i] < piv:
            i += 1

        j -= 1
        while a[j] > piv:
            j -= 1

        if i >= j:
            return j

        a[i], a[j] = a[j], a[i]
        draw_quick(a, i, j, lo)
        root.update()
        time.sleep(1.0)

def draw_quick(a, curr, compared, piv):
    """
    Function to draw the quick sort on the canvas 
    """
    canvas5.delete("all")
    left_margin = 15
    width = 50
    height = 50
    padding = 10

    for i, el in enumerate(a):
        if i == curr:
            color = "chartreuse4"
        
        if i == piv:
            canvas5.create_text(300, 220, text=f"The current pivot is [{el}]", font=('Saab', 15, 'bold'))
            color = "red4"

        elif i == compared:
            color = "steel blue"
        else:
            color = "gray6"

        canvas5.create_rectangle(left_margin, 100, left_margin + width, 100 + height, fill=color)
        canvas5.create_text(left_margin + width // 2, 100 + height // 2, text=str(el), fill="snow")
        left_margin += width + padding


root = tk.Tk()
root.title("Sorting Algorithms Visualizer")

tabControl = ttk.Notebook(root) # Using the notebook widget to create seperate tabs

tab1 = ttk.Frame(tabControl)
tab2 = ttk.Frame(tabControl)
tab3 = ttk.Frame(tabControl)
tab4 = ttk.Frame(tabControl)
tab5 = ttk.Frame(tabControl)


tabControl.add(tab1, text='Bubble Sort')
tabControl.add(tab2, text='Selection Sort')
tabControl.add(tab3, text='Insertion Sort')
tabControl.add(tab4, text='Merge Sort')
tabControl.add(tab5, text='Quick Sort')

# Buttons 
# On each tab we have buttons that, when user clicks them, does the sorting or quitting job
ttk.Button(tab1, text="Sort", command=bubble_sort).grid(row=1, column=0)  # placing the sort buttons at row 1 to place them on the top
ttk.Button(tab1, text="Quit", command=tab1.quit).grid(row=1, column=1)

ttk.Button(tab2, text="Sort", command=selection_sort).grid(row=1, column=0) 
ttk.Button(tab2, text="Quit", command=tab2.quit).grid(row=1, column=1)

ttk.Button(tab3, text="Sort", command=insertion_sort).grid(row=1, column=0)
ttk.Button(tab3, text="Quit", command=tab3.quit).grid(row=1, column=1)

ttk.Button(tab4, text="Sort", command=merge_sort).grid(row=1, column=0)
ttk.Button(tab4, text="Quit", command=tab4.quit).grid(row=1, column=1)

ttk.Button(tab5, text="Sort", command=quick_sort).grid(row=1, column=0)
ttk.Button(tab5, text="Quit", command=tab5.quit).grid(row=1, column=1)


# Canvases
canvas = tk.Canvas(tab1, width=610, height=250, bg="grey")
canvas.grid(row=2, column=0, columnspan=2)

canvas2 = tk.Canvas(tab2, width=610, height=250, bg="grey")
canvas2.grid(row=2, column=0, columnspan=2)

canvas3 = tk.Canvas(tab3, width=610, height=250, bg="grey")
canvas3.grid(row=2, column=0, columnspan=2)

canvas4 = tk.Canvas(tab4, width=610, height=250, bg="grey")
canvas4.grid(row=2, column=0, columnspan=2)

canvas5 = tk.Canvas(tab5, width=610, height=250, bg="grey")
canvas5.grid(row=2, column=0, columnspan=2)

# Labels for each tab
ttk.Label(tab1, text="Bubble Sort Visualization").grid(row=0, column=0)
ttk.Label(tab2, text="Selection Sort Visualization").grid(row=0, column=0)
ttk.Label(tab3, text="Insertion Sort Visualization").grid(row=0, column=0)
ttk.Label(tab4, text="Merge Sort Visualization").grid(row=0, column=0)
ttk.Label(tab5, text="Quick Sort Visualization").grid(row=0, column=0)

# tabControl.pack is used to connect all tabs in one window
tabControl.pack(expand=1, fill="both")

# Needed at the end of every program that works with tkinter
root.mainloop()
