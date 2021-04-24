generic
	type T_Element is private;

package Liste is

	type T_Liste is private;

	Indice_Error: Exception;	-- Un indice est invalide
	Element_Absent_Error: Exception;	-- �l�ment non trouv�


	-- Initialiser une liste.
	procedure Initialiser (Liste: in out T_Liste) with
		Post => Taille (Liste) = 0;


	-- D�truire une liste et lib�rer toutes les ressources qu'elle utilise.
	-- Une liste d�truite ne doit plus �tre utilis�e.
	procedure Detruire (Liste: in out T_Liste);


	-- Ajouter un nouvel �l�ment au d�but d'une liste.
	procedure Ajouter(Liste: in out T_Liste; Element: T_Element) ;


	-- Retourner le premier �l�ment d'une liste.
	-- Exception : Element_Absent_Error si la liste est vide
	function Premier (Liste: in T_Liste) return T_Element;


	-- Retourner la taille d'une liste.
	function Taille (Liste: in T_Liste) return Integer;


	-- Retourner vrai ssi Element est dans Liste.
	function Est_Present (Liste: in T_Liste; Element: in T_Element) return Boolean;



private

	type T_Cellule;

	type T_Liste is access T_Cellule;

	type T_Cellule is record
			Element: T_Element;
			Suivant: T_Liste;
	end record;


end Liste;
