------Planta vs Zombies
data Planta = Planta {
	nombrePlanta :: String,
	puntosDeVida :: Int,
	cantidadDeSoles :: Int,
	nivelDeAtaque :: Int
} deriving (Show)

peaShooter = Planta {
	nombrePlanta = "PeaShooter",
	puntosDeVida = 5 ,
	cantidadDeSoles = 0,
	nivelDeAtaque = 2
}
repeater = Planta {
	nombrePlanta = "Repeater",
	puntosDeVida = 5 ,
	cantidadDeSoles = 0,
	nivelDeAtaque = nivelDeAtaque peaShooter * 2
}
sunflower = Planta {
	nombrePlanta = "Sunflower",
	puntosDeVida = 7 ,
	cantidadDeSoles = 5,
	nivelDeAtaque = 3
}
nut = Planta {
	nombrePlanta = "Nut",
	puntosDeVida = 100 ,
	cantidadDeSoles = 0,
	nivelDeAtaque = 0
}
girasol = Planta {
	nombrePlanta = "Girasol",
	puntosDeVida = puntosDeVida sunflower * 3 ,
	cantidadDeSoles = 10,
	nivelDeAtaque = 5
}
petaSeta = Planta {
	nombrePlanta ="PetaSeta",
	puntosDeVida = 70 ,
	cantidadDeSoles = 5,
	nivelDeAtaque = 10
}

data Zombies = Zombies {
	nombre :: String,
	accesorios :: [String],
	fuerzaMordida :: Int
} deriving (Show) 

basico = Zombies {
	nombre = "Zombie Base",
	accesorios = [],
	fuerzaMordida = 1
}
balloon = Zombies {
	nombre = "Zombie Ballon",
	accesorios = ["globo"],
	fuerzaMordida = 1
}
newspapers = Zombies {
	nombre = "Zombie Newspapers",
	accesorios = ["diario"],
	fuerzaMordida = 2
}
gargantuar = Zombies {
	nombre = "Zombie Gargantuar Hulls Smas Puny God",
	accesorios = ["poste electrico", "zombie enano"],
	fuerzaMordida = 30
}


nivelDeMuerte :: Zombies -> Int
nivelDeMuerte = length.nombre

cantidadDeAccesorios :: Zombies -> Int
cantidadDeAccesorios = length.accesorios 


especialidad :: Planta -> String
especialidad planta	|cantidadDeSoles planta > 0 = "Proveedora"
			|nivelDeAtaque planta > puntosDeVida planta = "Atacadora"
			|otherwise = "Defensiva"

esPeligroso :: Zombies -> Bool
esPeligroso zombie = nivelDeMuerte zombie > 1 || cantidadDeAccesorios zombie > 10 


data LineaDeDefensa = LineaDeDefensa {
	plantas :: [Planta],
	zombies :: [Zombies]
} deriving (Show) 

linea1 = LineaDeDefensa {
		plantas = [sunflower, sunflower, sunflower],
		zombies = []
		}
linea2 = LineaDeDefensa {
		plantas = [peaShooter, peaShooter, sunflower, nut],
		zombies = [basico, newspapers]
		}
linea3 = LineaDeDefensa {
		plantas = [sunflower],
		zombies = [gargantuar, basico, basico]
		}
agregarPlanta :: LineaDeDefensa -> Planta -> LineaDeDefensa
agregarPlanta lineaNueva unaPlanta = lineaNueva {plantas =  ((plantas lineaNueva): unaPlanta), zombies = zombies lineaNueva }

