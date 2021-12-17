with Ada.Text_IO, Ada.Integer_Text_IO;
use Ada.Text_IO, Ada.Integer_Text_IO;
with Sorting;
Use Sorting;

procedure MainProg is
    

    procedure ReadArr is
        x: Integer;
    begin
        for i in 1 .. SIZE loop
            Get(x);
            my_arr(i) := MyInt(x);
        end loop;
    end ReadArr;

    procedure PrintArr is
    begin
        for i in 1 .. SIZE loop
            Put(my_arr(i)'Image);
            Put(" ");
        end loop;
        New_Line;
    end PrintArr;

    task PrintingTask is
        entry Start;
        entry NoteFromSorting;
        entry NoteFromAdding(val: Integer);
    end PrintingTask;

    task AddingTask is
        entry Start;
        entry NoteFromSorting;
    end AddingTask;

    task SortingTask is
        entry Start;
        entry NoteFromPrinting;
    end SortingTask;

    task body PrintingTask is
    begin
        accept Start;
        my_lock.Seize;
        Put("array input: ");
        PrintArr;
        my_lock.Release;
        SortingTask.NoteFromPrinting;
        accept NoteFromSorting;
        my_lock.Seize;
        Put("array sorted: ");
        PrintArr;
        my_lock.Release;
        accept NoteFromAdding(val: Integer) do
            Put("SUM: ");
            Put(val'Image);
            New_Line;
        end NoteFromAdding;
    end PrintingTask;

    task body AddingTask is
        sum: Integer := 0;
    begin
        accept Start;
        accept NoteFromSorting;
        my_lock.Seize;
        for i in 1 .. SIZE loop
            sum := sum + Integer(my_arr(i));
        end loop;
        my_lock.Release;
        PrintingTask.NoteFromAdding(sum);
    end AddingTask;

    task body SortingTask is
    begin
        accept Start;
        accept NoteFromPrinting;
        my_lock.Seize;
        QSort(1, SIZE);
        my_lock.Release;
        PrintingTask.NoteFromSorting;
        AddingTask.NoteFromSorting;
    end SortingTask;

begin
    ReadArr;
    PrintingTask.Start;
    AddingTask.Start;
    SortingTask.Start;

end MainProg;