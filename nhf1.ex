defmodule Nhf1 do
  @moduledoc """
  Kemping
  @author "Kővári Dániel Máté <kovari.dani@gmail.com>"
  @date   "2022-10-14"
  ...
  """

  # sor száma (1-től n-ig)
  @type row :: integer
  # oszlop száma (1-től m-ig)
  @type col :: integer
  # egy parcella koordinátái
  @type field :: {row, col}

  # a sátrak száma soronként
  @type tents_count_rows :: [integer]
  # a sátrak száma oszloponként
  @type tents_count_cols :: [integer]

  # a fákat tartalmazó parcellák koordinátái lexikálisan rendezve
  @type trees :: [field]
  # a feladványleíró hármas
  @type puzzle_desc :: {tents_count_rows, tents_count_cols, trees}

  # a sátorpozíciók iránya: north, east, south, west
  @type dir :: :n | :e | :s | :w
  # a sátorpozíciók irányának listája a fákhoz képest
  @type tent_dirs :: [dir]

  # tree and the according tent direction
  @type tree_and_direction :: {field, dir}

  # a fák száma a kempingben
  @type cnt_tree :: integer
  # az elemek száma a sátorpozíciók irányának listájában
  @type cnt_tent :: integer
  # a sátrak száma rossz a felsorolt sorokban
  @type err_rows :: %{err_rows: [integer]}
  # a sátrak száma rossz a felsorolt oszlopokban
  @type err_cols :: %{err_cols: [integer]}
  # a felsorolt koordinátájú sátrak másikat érintenek
  @type err_touch :: %{err_touch: [field]}
  # hibaleíró hármas
  @type errs_desc :: {err_rows, err_cols, err_touch}

  @spec satrak(pd :: puzzle_desc) :: tss :: [tent_dirs]
  # tss a pd feladványleíróval megadott feladvány összes megoldásának listája, tetszőleges sorrendben
  def satrak(pd) do
    traverse_trees(elem(pd, 2), [], pd, elem(pd, 0), elem(pd, 1))
  end

  # recursively traverse through the trees, generating all possible tent directions and checking them
  @spec traverse_trees(
          remaining_trees :: trees,
          tent_directions :: tent_dirs,
          puzzle_description :: puzzle_desc,
          remaining_positions_row :: tents_count_rows,
          remaining_positions_col :: tents_count_cols
        ) ::
          tent_direction :: [tent_dirs]

  # if at the end of recursion
  def traverse_trees(
        [],
        tent_directions,
        puzzle_description,
        _,
        _
      ) do
    a = check_sol(puzzle_description, tent_directions)
    # if there is no error

    if a == {%{err_rows: []}, %{err_cols: []}, %{err_touch: []}} do
      # return the tent directions
      [tent_directions]
    else
      # else return an empty list
      []
    end
  end

  def traverse_trees(
        remaining_trees,
        tent_directions,
        puzzle_description,
        remaining_positions_row,
        remaining_positions_col
      ) do
    # generating the possible tent positions while also checking if the tent is not out of bounds
    tree_directions =
      [:w, :n, :e, :s]
      |> Enum.filter(fn dir ->
        is_direction_valid?(
          hd(remaining_trees),
          dir,
          puzzle_description,
          tent_directions,
          remaining_positions_row,
          remaining_positions_col
        )
      end)

    tree_directions
    |> Enum.map(fn dir ->
      # gets the coordinates of the given tent
      {{x, y}, _} = direction_shift(hd(remaining_trees), dir)

      remaining_positions_row = decrement_at(remaining_positions_row, x)
      remaining_positions_col = decrement_at(remaining_positions_col, y)

      traverse_trees(
        tl(remaining_trees),
        tent_directions ++ [dir],
        puzzle_description,
        remaining_positions_row,
        remaining_positions_col
      )
    end)
    |> Enum.reduce([], fn x, acc -> acc ++ x end)
  end

  # decrements the list value at a given index by 1 (used remaining_positions_col/row)
  defp decrement_at(list, index) do
    List.replace_at(list, index - 1, Enum.at(list, index - 1) - 1)
  end

  # checks if the given direction is not outside of map and not already occupied
  defp is_direction_valid?(
         tree,
         direction,
         puzzle_description,
         tent_directions,
         remaining_positions_row,
         remaining_positions_col
       ) do
    n = length(elem(puzzle_description, 0))
    m = length(elem(puzzle_description, 1))

    trees = elem(puzzle_description, 2)

    {{x, y}, _} = direction_shift(tree, direction)

    # if the position exceeds column or row counts
    if Enum.at(remaining_positions_row, x - 1) == 0 or
         Enum.at(remaining_positions_col, y - 1) == 0 do
      false
    end

    # check if the current coordinate is already occupied by a generated tent
    coordinate_is_occupied =
      Enum.zip(trees, tent_directions)
      |> Enum.map(fn {tree, direction} ->
        elem(direction_shift(tree, direction), 0)
      end)
      |> Enum.member?({x, y})

    if coordinate_is_occupied do
      false
    else
      # current coordinate is not occupied by another tent and it's not outside of the map
      if(x >= 1 and x <= n and y >= 1 and y <= m) do
        true
      else
        false
      end
    end
  end

  # from khf3 module
  @spec check_sol(pd :: puzzle_desc, ds :: tent_dirs) :: ed :: errs_desc
  def check_sol({tents_count_rows, tents_count_cols, trees}, tent_dirs) do
    tree_list = trees |> Enum.map(fn x -> {x, :tree} end)

    tent_positions =
      tree_list
      |> Enum.zip(tent_dirs)
      |> Enum.map(fn {{tree, _}, dir} -> direction_shift(tree, dir) end)

    empty_grid =
      Enum.map(1..length(tents_count_rows), fn x ->
        Enum.map(1..length(tents_count_cols), fn y -> {{x, y}, :empty} end)
      end)

    grid =
      [tree_list ++ tent_positions ++ empty_grid]
      |> List.flatten()
      # uniq by coordinates means that the redundant empty field are popped
      |> Enum.uniq(fn {{x, y}, _} -> {x, y} end)
      # sorts by coordinates
      |> Enum.sort_by(fn {{x, y}, _} -> {x, y} end)

    err_rows =
      tents_count_rows
      |> Enum.with_index(1)
      |> Enum.map(fn {value, index} ->
        # checks number of tents are equeal to expected number of tents in row
        true_or_false =
          Enum.filter(grid, fn {{x, _}, type} -> x == index && type == :tent end) |> length() ==
            value

        # negative number means we have no information about the tents in row
        if value < 0 || true_or_false do
          nil
        else
          index
        end
      end)
      |> Enum.reject(fn x -> x == nil end)

    err_cols =
      tents_count_cols
      |> Enum.with_index(1)
      |> Enum.map(fn {value, index} ->
        # checks number of tents are equeal to expected number of tents in columns
        true_or_false =
          Enum.filter(grid, fn {{_, y}, type} -> y == index && type == :tent end) |> length() ==
            value

        # negative number means we have no information about the tents in columns
        if value < 0 || true_or_false do
          nil
        else
          index
        end
      end)
      |> Enum.reject(fn x -> x == nil end)

    tent_coordinates = tent_positions |> Enum.map(fn {x, _} -> x end)

    err_touch =
      tent_positions
      |> Enum.map(fn {{x1, y1}, _} ->
        # initalizes a 3x3 grid around the current tent's location
        possible_neighboring_tents =
          Enum.map((x1 - 1)..(x1 + 1), fn x2 ->
            Enum.map((y1 - 1)..(y1 + 1), fn y2 -> {x2, y2} end)
          end)
          |> List.flatten()
          |> Enum.reject(fn {x, y} -> {x, y} == {x1, y1} end)

        # checks if tents are in the touching area of other tents
        true_or_false =
          possible_neighboring_tents
          |> Enum.any?(fn {x, y} -> {x, y} in tent_coordinates end)

        {{x1, y1}, true_or_false}
      end)
      |> Enum.filter(fn {{_, _}, value} -> value == true end)
      |> Enum.map(fn {{x, y}, _} -> {x, y} end)
      |> Enum.sort()

    {%{err_rows: err_rows}, %{err_cols: err_cols}, %{err_touch: err_touch}}
  end

  # Az {rs, cs, ts} = pd feladványleíró és a ds sátorirány-lista
  # alapján elvégzett ellenőrzés eredménye a cd hibaleíró, ahol
  #   rs a sátrak soronként elvárt számának a listája,
  #   cs a sátrak oszloponként elvárt számának a listája,
  #   ts a fákat tartalmazó parcellák koordinátájának a listája
  # Az {e_rows, e_cols, e_touch} = ed hármas elemei olyan
  # kulcs-érték párok, melyekben a kulcs a hiba jellegére utal, az
  # érték pedig a hibahelyeket felsoroló lista (üres, ha nincs hiba)

  # Disclaimer: Reused from the previous homework
  # Shifts the coordinates into the desired direction by the alignment
  @spec direction_shift(position :: field, direction :: dir) :: f :: {field, :tent}
  def direction_shift(position, direction) do
    {row, col} = position

    case direction do
      :n -> {{row - 1, col}, :tent}
      :e -> {{row, col + 1}, :tent}
      :s -> {{row + 1, col}, :tent}
      :w -> {{row, col - 1}, :tent}
    end
  end
end

defmodule Khf1 do
  @moduledoc """
  Kemping
  @author "Kővári Dániel Máté <kovari.dani@gmail.com>"
  @date   "2022-09-23"
  ...
  """

  # sor száma (1-től n-ig)
  @type row :: integer
  # oszlop száma (1-től m-ig)
  @type col :: integer
  # egy parcella koordinátái
  @type field :: {row, col}

  # a sátrak száma soronként
  @type tents_count_rows :: [integer]
  # a sátrak száma oszloponként
  @type tents_count_cols :: [integer]

  # a fákat tartalmazó parcellák koordinátái lexikálisan rendezve
  @type trees :: [field]
  # a feladványleíró hármas
  @type puzzle_desc :: {tents_count_rows, tents_count_cols, trees}

  @spec to_internal(file :: String.t()) :: pd :: puzzle_desc
  # A file fájlban szövegesen ábrázolt feladvány leírója pd
  def to_internal(file) do
    elements = split_into_elements(file)

    # first row of the input file
    first_row =
      elements
      |> List.first()
      |> Enum.map(fn x -> String.to_integer(x) end)

    # first column of the input file
    first_column =
      elements
      |> Enum.slice(1..-1)
      |> Enum.map(fn x -> List.first(x) end)
      |> Enum.map(fn x -> String.to_integer(x) end)

    # gets the tents (marked as *) in the input file
    tents =
      elements
      # removes the first row
      |> Enum.reject(fn x -> x == first_row end)
      # removes the first column
      |> Enum.map(fn x -> Enum.reject(x, fn y -> y == List.first(x) end) end)
      |> Enum.with_index()
      # assigns the row elements with indices, then maps the "coordinates" into the touples
      |> Enum.map(fn {x, i} ->
        Enum.with_index(x) |> Enum.map(fn {y, j} -> {y, i, j + 1} end)
      end)
      |> List.flatten()
      # filters out the tents
      |> Enum.filter(fn {x, _, _} -> x == "*" end)
      # removes the "*" from the touples
      |> Enum.map(fn {_, i, j} -> {i, j} end)

    # creates a touple containing the first column, first row and the tents
    pd = {first_column, first_row, tents}
    pd
  end

  # Split the readfile into elements: split into rows -> trimmed the whitespaces
  # -> removed the empty elements -> split elements within line
  @spec split_into_elements(read_file :: String.t()) :: elements :: [[String.t()]]
  defp split_into_elements(read_file) do
    File.read!(read_file)
    |> String.split("\n", trim: true)
    |> Enum.map(fn x -> String.trim(x) end)
    |> Enum.reject(fn x -> x == "" end)
    |> Enum.map(fn x -> String.split(x, " ", trim: true) end)
  end
end
