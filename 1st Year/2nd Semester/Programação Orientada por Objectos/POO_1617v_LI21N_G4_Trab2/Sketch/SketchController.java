package app.Sketch;

import android.app.Activity;
import android.os.Bundle;
import android.util.Log;
import android.view.MotionEvent;
import android.view.View;
import android.widget.Button;
import android.widget.LinearLayout;
import android.widget.RadioButton;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;

import app.Sketch.model.*;
import app.Sketch.view.*;

public class SketchController extends Activity {
    final private String MODELFILENAME ="model.txt";
    final private String BUTTON_RESET="RESET";
    final private String BUTTON_LOAD="LOAD";
    final private String BUTTON_SAVE="SAVE";
    // model
    public Sketch model;
    // controllers
    char selected;
    // view
    SketchView view;

    @Override
    protected void onStart() {
        super.onStart();
    }

    @Override
    protected void onResume() {
        super.onResume();
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_sketch);

        //model init
        model = new Sketch();

        //Layout
        final LinearLayout layout_buttons = new LinearLayout(this);
        final Button[] bt = new Button[3];

        final LinearLayout layout_radiobuttons = new LinearLayout(this);
        final RadioButton[] rb = new RadioButton[4];

        View.OnClickListener listener_radiobuttons = new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                // update model
                for (RadioButton other : rb) {
                    if (other == v) {
                        other.setChecked(true);
                        selected = other.getText().charAt(0);
                    } else other.setChecked(false);
                }
                // refresh view
                Log.i("@RADIO BUTTONS", "" + selected + " is the char selected");
            }
        };


        View.OnClickListener listener_buttons = new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                try {
                    switch ((String) ((Button) v).getText()) {
                        case BUTTON_RESET:
                            onReset();
                            break;
                        case BUTTON_LOAD:
                            onLoad();
                            break;
                        case BUTTON_SAVE:
                            onSave();
                            break;
                        default:
                    }
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        };

        bt[0] = new Button(this);
        bt[0].setText(BUTTON_RESET);
        bt[1] = new Button(this);
        bt[1].setText(BUTTON_LOAD);
        bt[2] = new Button(this);
        bt[2].setText(BUTTON_SAVE);

        for (Button b : bt) {
            b.setOnClickListener(listener_buttons);
            layout_buttons.addView(b);
        }

        rb[0] = new RadioButton(this);
        rb[0].setText("Line");
        rb[1] = new RadioButton(this);
        rb[1].setText("Rect");
        rb[2] = new RadioButton(this);
        rb[2].setText("Pixel");
        rb[3] = new RadioButton(this);
        rb[3].setText("Circle");

        for (RadioButton r : rb) {
            r.setOnClickListener(listener_radiobuttons);
            layout_radiobuttons.addView(r);
        }

        rb[0].toggle();
        this.selected = 'L';

        view = new SketchView(this);

        LinearLayout all = new LinearLayout(this);
        all.setOrientation(LinearLayout.VERTICAL);
        all.addView(layout_buttons);
        all.addView(layout_radiobuttons);
        all.addView(view);
        setContentView(all);

        view.setOnTouchListener(new View.OnTouchListener() {
            @Override
            public boolean onTouch(View v, MotionEvent event) {
                if (event.getAction() == MotionEvent.ACTION_DOWN)
                    model.add( Figure.GetFigureFromLetter(selected,(int) event.getX(), (int) event.getY()));
                if (event.getAction() == MotionEvent.ACTION_MOVE)
                    model.getLastCreated().setEnd((int) event.getX(), (int) event.getY());
                view.OnTouchEvent(event);
                return true;
            }
        });
    }

    private void onReset(){
        model.removeAll();
        view.clear();
    }

    private void onLoad() throws IOException {
        FileInputStream in = openFileInput(MODELFILENAME);
        model.load(in);
        in.close();
        view.reload(model);
    }

    private void onSave() throws IOException {
        FileOutputStream out  = openFileOutput(MODELFILENAME, MODE_PRIVATE);
        model.save(out);
        out.close();
        /*
        File sd = Environment.getExternalStorageDirectory();
        String path = sd.getAbsolutePath();
        Log.e("@filepath", path);
        */
    }
}
