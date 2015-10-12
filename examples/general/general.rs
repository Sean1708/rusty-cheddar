#![feature(plugin)]
#![plugin(cheddar)]

cheddar! {"general.h"
    enum Eye {
        Blue,
        Green,
        Red,
    }

    struct Person {
        age: i8,
        weight: f32,
        height: f32,
    }

    fn Person_create(age: i8, weight_lbs: f32, height_ins: f32) -> Person {
        Person {
            age: age,
            weight: weight_lbs * 0.45,
            height: height_ins * 0.0254,
        }
    }

    fn Person_describe(person: Person, eyes: Eye) {
        let eyes = match eyes {
            Eye::Blue => "blue",
            Eye::Green => "green",
            Eye::Red => "red",
        };
        println!(
            "The {}m {} year old weighed {}kg and had {} eyes.",
            person.height, person.age, person.weight, eyes,
        );
    }
}
