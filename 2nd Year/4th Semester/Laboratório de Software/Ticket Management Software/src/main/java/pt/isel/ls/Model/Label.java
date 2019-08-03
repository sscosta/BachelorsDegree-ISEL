package pt.isel.ls.Model;

public class Label {

    public String description;
    public String color;

    public Label(String description,String color) {
        this.description = description;
        this.color=color;
    }

    public String getDescription() {
        return this.description;
    }

    @Override
    public String toString() {
        return "Label:\n" +
                "\tDescription -> " + description + "\n" +
                "\tColor -> " + color;
    }

    public static final class LabelBuilder {
        String description;
        String color;

        public LabelBuilder() {
        }

        public static LabelBuilder aLabel() {
            return new LabelBuilder();
        }

        public LabelBuilder withDescription(String description) {
            this.description = description;
            return this;
        }

        public LabelBuilder withColor(String color){
            this.color = color;
            return this;
        }

        public Label build() {
            return new Label(description,color);
        }
    }
}
