-- Ce module dÃ©finit un type Vecteur_Creux et les opÃ©rations associÃ©s. Un
-- vecteur creux est un vecteur qui contient essentiellement des 0. Aussi pour
-- Ã©conomiser l'espace de stockage et les temps de calculs, on ne conserve que



package Vecteurs_Creux is

	type T_Vecteur_Creux is limited private;
        type T_Double is digits 6;

	-- Initialiser un vecteur creux.  Il est nul.
	procedure Initialiser (V : out T_Vecteur_Creux) with
		Post => Est_Nul (V);


	-- DÃ©truire le vecteur V.
	procedure Detruire (V: in out T_Vecteur_Creux);


	-- Est-ce que le vecteur V est nul ?
	function Est_Nul (V : in T_Vecteur_Creux) return Boolean;


	-- RÃ©cupÃ©rer la composante (valeur) du vecteur V Ã  l'indice Indice.
	function Composante_Recursif (V : in T_Vecteur_Creux ; Indice : in Integer) return T_Double
		with Pre => Indice >= 1;


	-- RÃ©cupÃ©rer la composante (valeur) du vecteur V Ã  l'indice Indice.
	function Composante_Iteratif (V : in T_Vecteur_Creux ; Indice : in Integer) return T_Double
		with Pre => Indice >= 1;


	-- Modifier une composante (Indice, Valeur) d'un vecteur creux.
	procedure Modifier (V : in out T_Vecteur_Creux ;
				       Indice : in Integer ;
					   Valeur : in T_Double ) with
		pre => Indice >= 1,
		post => Composante_Recursif (V, Indice) = Valeur;


	-- Est-ce que deux vecteurs creux sont Ã©gaux ?
	function Sont_Egaux_Recursif (V1, V2 : in T_Vecteur_Creux) return Boolean;


	-- Est-ce que deux vecteurs creux sont Ã©gaux ?
	function Sont_Egaux_Iteratif (V1, V2 : in T_Vecteur_Creux) return Boolean;

	-- Nombre de composantes non nulles du vecteur V.
	--
	-- Ce sous-programme ne fait normalement pas partie de la spÃ©cification
	-- du module.  Il a Ã©tÃ© ajoutÃ© pour faciliter l'Ã©criture des programmes
	-- de test.
	function Nombre_Composantes_Non_Nulles (V: in T_Vecteur_Creux) return Integer with
		Post => Nombre_Composantes_Non_Nulles'Result >= 0;


private

	type T_Cellule is
		record
			Indice : Integer;
			Valeur : T_Double;
			Suivant : T_Vecteur_Creux;
			-- Invariant :
			--   Indice >= 1;
			--   Suivant = Null or else Suivant.all.Indice > Indice;
			--   	-- les cellules sont stockÃ©s dans l'ordre croissant des indices.
                end record;

        type T_Vecteur_Creux is access T_Cellule;

end Vecteurs_Creux;

