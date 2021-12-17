with Ada.Text_IO; use Ada.Text_IO;
package body Sorting is
    protected body MyLock is
        entry Seize when not Busy is
        begin
            Busy := True;
        end Seize;

        procedure Release is
        begin
            Busy := False;
        end Release;
    end MyLock;

    procedure QSort(Low: Integer; High: Integer) is
        mid_i: Integer;
        low_val: Integer;
        high_val: Integer;
        mid_val: Integer;
        median: Integer;
        i: Integer := Low;
        j: Integer := High;
        tmp: MyInt;

        task type SortTaskType is
            entry Start(lowi: Integer; highi: Integer);
            entry AbortTask;
        end SortTaskType;

        task body SortTaskType is 
            low_i: Integer;
            high_i: Integer;
        begin
            select 
                accept Start(lowi: Integer; highi: Integer) do
                    low_i := lowi;
                    high_i := highi;
                end Start;
                QSort(low_i, high_i);
            or
                accept AbortTask;
            end select;    
        end SortTaskType;

        T1: SortTaskType;
        T2: SortTaskType;
    begin
        if High - Low <= 0 then
            T1.AbortTask;
            T2.AbortTask;
        elsif High - Low = 1 then
            T1.AbortTask;
            T2.AbortTask;
            low_val := Integer(my_arr(Low));
            high_val := Integer(my_arr(High));
            if low_val <= high_val then
                null;
            else
                tmp := my_arr(Low);
                my_arr(Low) := my_arr(High);
                my_arr(High) := tmp;
            end if;
        else
            mid_i := (Low + High)/2;
            low_val := Integer(my_arr(Low));
            high_val := Integer(my_arr(High));
            mid_val := Integer(my_arr(mid_i));
            median := low_val + high_val + mid_val - Integer'Max(low_val, Integer'Max(mid_val, high_val)) 
                - Integer'Min(low_val, Integer'Min(mid_val, high_val));
            while i < j loop
                while Integer(my_arr(i)) <= median loop
                    i := i + 1;
                end loop;
                exit when i >= j;
                while Integer(my_arr(j)) > median loop
                    j := j - 1;
                end loop;
                exit when i >= j;
                tmp := my_arr(i);
                my_arr(i) := my_arr(j);
                my_arr(j) := tmp;
            end loop;
            T1.Start(Low, i - 1);
            T2.Start(i, High);  
        end if;
    end QSort;
end Sorting;