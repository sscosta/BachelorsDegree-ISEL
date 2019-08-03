package pt.isel.leic.poo.app;

import android.app.Activity;
import android.os.Bundle;
import android.view.View;
import android.widget.Button;

public class MenuActivity extends Activity {
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_menu);

        View.OnClickListener selectLvl = new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                GameActivity.startNewGameFromLevel(MenuActivity.this, Integer.parseInt((String) v.getTag()));
            }
        };
        Button button;
        button =((Button) findViewById(R.id.level1));
        button.setText(getString(R.string.Level_text,1));
        button.setOnClickListener(selectLvl);
        button =((Button) findViewById(R.id.level2));
        button.setText(getString(R.string.Level_text,2));
        button.setOnClickListener(selectLvl);
        button =((Button) findViewById(R.id.level3));
        button.setText(getString(R.string.Level_text,3));
        button.setOnClickListener(selectLvl);
        button =((Button) findViewById(R.id.level4));
        button.setText(getString(R.string.Level_text,4));
        button.setOnClickListener(selectLvl);
    }
}
