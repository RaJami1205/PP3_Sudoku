using Microsoft.AspNetCore.Mvc.RazorPages;
using System.Diagnostics;
using System.Text.Json;

namespace sudoku.Pages.Tablero {
	public class tableroModel : PageModel {
		public int?[,] Board { get; set; } = new int?[9, 9];
		public int?[,] OriginalBoard { get; set; } = new int?[9, 9];
		public int?[,] SolutionBoard { get; set; } = new int?[9, 9];
		public Dictionary<(int, int), string> CellColors { get; set; } = new();
		public bool MostrarColores { get; set; } = false;

		private List<List<int?>> ConvertToList(int?[,] matrix) {
			var list = new List<List<int?>>();
			for (int i = 0; i < 9; i++) {
				var row = new List<int?>();
				for (int j = 0; j < 9; j++) {
					row.Add(matrix[i, j]);
				}
				list.Add(row);
			}
			return list;
		}

		private int?[,] ConvertToMatrix(List<List<int?>> list) {
			var matrix = new int?[9, 9];
			for (int i = 0; i < 9; i++) {
				for (int j = 0; j < 9; j++) {
					matrix[i, j] = list[i][j];
				}
			}
			return matrix;
		}

		public void OnGet() {
			TempData["PistasUsadas"] = 0;
			ViewData["PistasUsadas"] = 0;

			try {
				var output = RunPrologAndGetBoard();
				if (output != null) {
					Board = output;
					OriginalBoard = (int?[,]) output.Clone();
				}
				TempData["OriginalBoard"] = JsonSerializer.Serialize(ConvertToList(OriginalBoard));
				TempData["SolutionBoard"] = JsonSerializer.Serialize(ConvertToList(SolutionBoard));


			} catch (Exception ex) {
				TempData["Error"] = $"Error al generar el tablero: {ex.Message}";
			}
		}

		public class SudokuJson {
			public int?[][] Puzzle { get; set; }
			public int?[][] Solution { get; set; }
		}

		private int?[,]? RunPrologAndGetBoard() {
			var psi = new ProcessStartInfo {
				FileName = @"swipl",
				Arguments = "-q -f backend/sudoku.pl -g generate_board_json -t halt",
				RedirectStandardOutput = true,
				UseShellExecute = false,
				CreateNoWindow = true
			};

			using (var process = Process.Start(psi)) {
				string output = process.StandardOutput.ReadToEnd();
				process.WaitForExit();

				output = output.Trim();
				if (output.StartsWith("json"))
					output = output.Substring(4);
				output = output.Replace("puzzle", "\"puzzle\"").Replace("solution", "\"solution\"");


				try {

					var options = new JsonSerializerOptions { PropertyNameCaseInsensitive = true };

					var json = JsonSerializer.Deserialize<SudokuJson>(output, options);

					if (json == null || json.Puzzle == null || json.Solution == null) return null;



					int?[,] puzzleBoard = new int?[9, 9];
					int?[,] solutionBoard = new int?[9, 9];

					for (int i = 0; i < 9; i++) {
						for (int j = 0; j < 9; j++) {
							puzzleBoard[i, j] = json.Puzzle[i][j];
							solutionBoard[i, j] = json.Solution[i][j];
						}
					}

					SolutionBoard = solutionBoard;
					return puzzleBoard;
				} catch (JsonException ex) {
					TempData["Error"] = $"JSON inválido desde Prolog: {ex.Message}";
					return null;
				}
			}
		}



		public void OnPost(List<List<int?>> inputs, string action) {
			MostrarColores = false;
			CellColors.Clear();

			// Recuperar OriginalBoard y SolutionBoard de TempData
			if (TempData.ContainsKey("OriginalBoard")) {
				var originalList = JsonSerializer.Deserialize<List<List<int?>>>(TempData["OriginalBoard"]!.ToString());
				OriginalBoard = ConvertToMatrix(originalList!);
				TempData.Keep("OriginalBoard");
			}

			if (TempData.ContainsKey("SolutionBoard")) {
				var solutionList = JsonSerializer.Deserialize<List<List<int?>>>(TempData["SolutionBoard"]!.ToString());
				SolutionBoard = ConvertToMatrix(solutionList!);
				TempData.Keep("SolutionBoard");
			}
			// Actualizar el tablero con inputs
			for (int i = 0; i < 9; i++) {
				for (int j = 0; j < 9; j++) {
					Board[i, j] = inputs[i][j];
				}
			}

			switch (action) {
				case "Verificar":
					MostrarColores = true;
					for (int i = 0; i < 9; i++) {
						for (int j = 0; j < 9; j++) {
							if (inputs[i][j].HasValue) {
								if (inputs[i][j] == SolutionBoard[i, j]) {
									CellColors[(i, j)] = "correct";
								} else {
									CellColors[(i, j)] = "incorrect";
								}
							}
						}
					}
					break;

				case "Pista":
					int pistasUsadas = TempData.ContainsKey("PistasUsadas") ? int.Parse(TempData["PistasUsadas"]!.ToString()!) : 0;

					if (pistasUsadas >= 5) {
						TempData["Error"] = "¡Ya usaste las 5 pistas permitidas!";
						break;
					}

					// Buscar todas las celdas modificables que estén vacías o incorrectas
					var posiblesCeldas = new List<(int, int)>();
					for (int i = 0; i < 9; i++) {
						for (int j = 0; j < 9; j++) {
							if (!OriginalBoard[i, j].HasValue && Board[i, j] != SolutionBoard[i, j]) {
								posiblesCeldas.Add((i, j));
							}
						}
					}

					if (posiblesCeldas.Count > 0) {
						var random = new Random();
						var (fila, col) = posiblesCeldas[random.Next(posiblesCeldas.Count)];
						Board[fila, col] = SolutionBoard[fila, col];

						// Aumentar el contador y mantener TempData
						pistasUsadas++;
						TempData["PistasUsadas"] = pistasUsadas;
						ViewData["PistasUsadas"] = pistasUsadas;

					}

					TempData.Keep("OriginalBoard");
					TempData.Keep("SolutionBoard");
					TempData.Keep("PistasUsadas");
					break;

				case "MostrarSolucion":
					Board = (int?[,]) SolutionBoard.Clone();
					break;

				case "Reiniciar":
					Board = (int?[,]) OriginalBoard.Clone();
					break;
			}
		}
	}
}
