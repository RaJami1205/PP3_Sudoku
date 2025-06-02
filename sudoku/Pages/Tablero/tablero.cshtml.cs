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
		public EstadisticasJuego Estadisticas { get; set; } = new();

		public class EstadisticasJuego {
			public int CeldasIngresadas { get; set; } = 0;
			public int Verificaciones { get; set; } = 0;
			public int ErroresVerificacion { get; set; } = 0;
			public int SugerenciasUsadas { get; set; } = 0;
			public string TipoFinalizacion { get; set; } = "abandono"; // "exitosa", "abandono", "autosolución"
		}
		public class SudokuJson {
			public int?[][] Puzzle { get; set; }
			public int?[][] Solution { get; set; }
		}

		/// <summary>
		/// Convierte una matriz 9x9 de enteros nullable en una lista de listas de enteros nullable.
		/// </summary>
		/// <param name="matrix">Matriz 9x9 de enteros nullable.</param>
		/// <returns>Lista de listas equivalente a la matriz de entrada.</returns>
		/// <remarks>La matriz debe tener tamaño 9x9; no se valida tamaño.</remarks>
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

		/// <summary>
		/// Convierte una lista de listas de enteros nullable en una matriz 9x9 de enteros nullable.
		/// </summary>
		/// <param name="list">Lista de listas de enteros nullable con tamaño 9x9.</param>
		/// <returns>Matriz 9x9 equivalente a la lista de entrada.</returns>
		/// <remarks>Se asume que la lista tiene exactamente 9 filas y 9 columnas; no se valida tamaño.</remarks>
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
			HttpContext.Session.SetString("EstadisticaActual", JsonSerializer.Serialize(new EstadisticasJuego()));
			Estadisticas = new EstadisticasJuego();
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

		/// <summary>
		/// Ejecuta un proceso externo de Prolog para generar un tablero de Sudoku y obtener el resultado en formato JSON.
		/// </summary>
		/// <returns>
		/// Matriz 9x9 nullable que representa el tablero generado si la ejecución es exitosa y el JSON es válido;
		/// null en caso de error o salida inválida.
		/// </returns>
		/// <remarks>
		/// Ejecuta SWI-Prolog con el archivo "backend/sudoku.pl" y la consulta "generate_board_json".
		/// El JSON esperado contiene propiedades "puzzle" y "solution".
		/// </remarks>
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

		/// <summary>
		/// Lee la lista completa de estadísticas almacenadas en sesión.
		/// </summary>
		/// <returns>Lista de objetos EstadisticasJuego. Retorna lista vacía si no existen datos.</returns>
		private List<EstadisticasJuego> LeerEstadisticasTodas() {
			var json = HttpContext.Session.GetString("EstadisticasTodas");
			if (string.IsNullOrEmpty(json)) return new List<EstadisticasJuego>();
			return JsonSerializer.Deserialize<List<EstadisticasJuego>>(json) ?? new List<EstadisticasJuego>();
		}

		/// <summary>
		/// Guarda en sesión una lista completa de estadísticas.
		/// </summary>
		/// <param name="lista">Lista de EstadisticasJuego a almacenar.</param>
		private void GuardarEstadisticasTodas(List<EstadisticasJuego> lista) {
			HttpContext.Session.SetString("EstadisticasTodas", JsonSerializer.Serialize(lista));
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
			// Cargar estadísticas desde sesión
			var estadisticasJson = HttpContext.Session.GetString("EstadisticaActual");
			var estadisticas = string.IsNullOrEmpty(estadisticasJson)
				? new EstadisticasJuego()
				: JsonSerializer.Deserialize<EstadisticasJuego>(estadisticasJson!);

			// Contar celdas ingresadas
			estadisticas.CeldasIngresadas = inputs.SelectMany(row => row).Count(val => val.HasValue);

			switch (action) {
				case "Verificar":
					MostrarColores = true;
					bool completado = true;

					for (int i = 0; i < 9; i++) {
						for (int j = 0; j < 9; j++) {
							if (inputs[i][j].HasValue) {
								if (inputs[i][j] == SolutionBoard[i, j]) {
									CellColors[(i, j)] = "correct";
								} else {
									CellColors[(i, j)] = "incorrect";
									completado = false;
									estadisticas.ErroresVerificacion++;
								}
							} else {
								completado = false;
							}
						}
					}

					estadisticas.Verificaciones++;
					var lista = LeerEstadisticasTodas();

					if (completado) {
						estadisticas.TipoFinalizacion = "exitosa";
						TempData["MostrarEstadisticas"] = true;

						lista.Add(estadisticas);
						GuardarEstadisticasTodas(lista);
					}
					break;

				case "Pista":
					int pistasUsadas = TempData.ContainsKey("PistasUsadas") ? int.Parse(TempData["PistasUsadas"]!.ToString()!) : 0;

					if (pistasUsadas >= 5) {
						TempData["Error"] = "¡Ya usaste las 5 pistas permitidas!";
						break;
					}

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

						pistasUsadas++;
						TempData["PistasUsadas"] = pistasUsadas;
						ViewData["PistasUsadas"] = pistasUsadas;

						estadisticas.SugerenciasUsadas++;
					}

					TempData.Keep("PistasUsadas");
					break;

				case "MostrarSolucion":
					Board = (int?[,]) SolutionBoard.Clone();
					estadisticas.TipoFinalizacion = "autosolución";
					TempData["MostrarEstadisticas"] = true;

					var listaSolucion  = LeerEstadisticasTodas();
					listaSolucion.Add(estadisticas);
					GuardarEstadisticasTodas(listaSolucion);

					break;


				case "Reiniciar":
					Board = (int?[,]) OriginalBoard.Clone();
					break;

				case "NuevoJuego":
					// Guardar estadística actual como "abandono"
					estadisticas.TipoFinalizacion = "abandono";

					var listaEstadisticas = LeerEstadisticasTodas();
					listaEstadisticas.Add(estadisticas);
					GuardarEstadisticasTodas(listaEstadisticas);

					// Generar nuevo tablero igual que en OnGet
					var nuevoTablero = RunPrologAndGetBoard();

					if (nuevoTablero != null) {
						Board = nuevoTablero;
						OriginalBoard = (int?[,]) nuevoTablero.Clone();

					} else {
						TempData["Error"] = "Error al generar un nuevo tablero.";
					}

					// Reiniciar estadísticas para el nuevo juego
					estadisticas = new EstadisticasJuego();
					TempData["PistasUsadas"] = 0;
					ViewData["PistasUsadas"] = 0;

					break;

			}
			Estadisticas = estadisticas;

			// Guardar estadísticas en sesión
			HttpContext.Session.SetString("EstadisticaActual", JsonSerializer.Serialize(estadisticas));
		}

	}
}
