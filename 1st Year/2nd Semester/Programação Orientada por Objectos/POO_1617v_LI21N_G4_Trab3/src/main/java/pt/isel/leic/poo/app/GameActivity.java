package pt.isel.leic.poo.app;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.view.View;
import android.widget.Button;
import android.widget.TextView;

import pt.isel.leic.poo.app.model.Game;
import pt.isel.leic.poo.app.view.cell.CellView;
import pt.isel.poo.tile.OnBeatListener;
import pt.isel.poo.tile.OnTileTouchListener;
import pt.isel.poo.tile.TilePanel;

public class GameActivity extends Activity  {
    private Game modelGame;

    private Button nextLevelButton;
    private TilePanel viewTilePanel;
    private TextView elapsedTimeTextView;
    private TextView levelTextView;

    private static final String SAVELEVELTXT="level";
    private static final String SAVETIMETXT="time";
    private static final String SAVEGAMETXT="game";

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        this.setContentView(R.layout.activity_game);

        this.nextLevelButton = (Button) this.findViewById(R.id.NextLevel);
        this.viewTilePanel = (TilePanel) this.findViewById(R.id.tilePanel);
        this.elapsedTimeTextView = (TextView) this.findViewById(R.id.timer);
        this.levelTextView= (TextView) this.findViewById(R.id.LEVEL_TEXT);

        this.modelGame = new Game(this.getAssets());

        this.nextLevelButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                GameActivity.startNewGameFromLevel(GameActivity.this,GameActivity.this.modelGame.getLevel()+1);
            }
        });

        this.viewTilePanel.setListener(new OnTileTouchListener(){

            @Override
            public boolean onClick(int xTile, int yTile) throws IllegalAccessException, InstantiationException {
                return  GameActivity.this.evaluateClick(yTile,xTile);
            }

            @Override
            public boolean onDrag(int xFrom, int yFrom, int xTo, int yTo) {
                return GameActivity.this.evaluateDrag(xFrom,yFrom,xTo,yTo);
            }

            @Override
            public void onDragEnd(int x, int y) {
                GameActivity.this.evaluateIsOver();
            }

            @Override
            public void onDragCancel() {
            }
        });

        this.viewTilePanel.setHeartbeatListener(1000,new OnBeatListener() {
            @Override
            public void onBeat(long beat, long time) {
                GameActivity.this.incrementTime();
            }
        });

        if(savedInstanceState!=null && savedInstanceState.containsKey(SAVELEVELTXT))
            this.modelGame.startGame(
                    savedInstanceState.getInt(SAVELEVELTXT),
                    savedInstanceState.getInt(SAVETIMETXT),
                    savedInstanceState.getString(SAVEGAMETXT)) ;
        else
            this.modelGame.startGame(this.getIntent().getIntExtra(SAVELEVELTXT,1));

        this.loadTilePanel();
        this.evaluateIsOver();
    }


    @Override
    protected void onSaveInstanceState(Bundle outState) {
        super.onSaveInstanceState(GameActivity.this.saveGame(outState));
    }

    /**
     * Save game in bundle
     * @param outstate bundle where to save game
     * @return
     */
    private Bundle saveGame(Bundle outstate){
        outstate.putInt(SAVELEVELTXT,this.modelGame.getLevel());
        outstate.putInt(SAVETIMETXT,this.modelGame.getElapsedTime());
        outstate.putString(SAVEGAMETXT,this.modelGame.saveToString());
        return outstate;
    }

    /**
     * increment elapsed time in game model
     */
    private void incrementTime(){
        this.modelGame.incrementElapsedTime();
        this.elapsedTimeTextView.setText(this.modelGame.elapsedTimeToString());
    }

    /**
     * load tileview to viewtilepanel
     */
    private void loadTilePanel() {
        this.levelTextView.setText( this.getString(R.string.Level_text,this.modelGame.getLevel()));
        this.elapsedTimeTextView.setText(this.modelGame.elapsedTimeToString());
        int width =this.modelGame.getWidth();
        int height=this.modelGame.getHeight();
        this.viewTilePanel.setSize(width,height);
        for (int line = 0; line< height; line++) {
            for (int col = 0; col < width; col++) {
                this.viewTilePanel.setTile(col, line,
                        CellView.newInstance(modelGame.getCellFromPosition(line,col)));
            }
        }
    }

    /**
     * start new activity with new game level
     * @param actual activity
     * @param level of game to start
     */
    public static void startNewGameFromLevel(Activity actual,int level){
        Intent toAnotherLevel = new Intent( actual, GameActivity.class);
        toAnotherLevel.putExtra(SAVELEVELTXT, level);
        actual.startActivity(toAnotherLevel);
    }

    /**
     * if game is over stop timer and show next level button
     */
    private void evaluateIsOver(){
        if(modelGame.isOver()) {
            this.viewTilePanel.removeHeartbeatListener();
            this.nextLevelButton.setVisibility(View.VISIBLE);
            this.nextLevelButton.invalidate();
        }
    }

    /**
     * on tile click unlink track
     * @param col of tile
     * @param line of tile
     * @return if unlink
     */
    private boolean evaluateClick( int col,int line){
        return this.modelGame.unlink(col,line);
    }

    /**
     * evaluate drag
     * @param colFrom of origin tile
     * @param lineFrom of origin tile
     * @param colTo of destination tile
     * @param lineTo of destination tile
     * @return if drag
     */
    private boolean evaluateDrag(int colFrom, int lineFrom, int colTo, int lineTo) {
        if (this.modelGame.drag(colFrom,lineFrom,colTo,lineTo)){
            this.viewTilePanel.invalidate(colFrom,lineFrom);
            this.viewTilePanel.invalidate(colTo,lineTo);
            this.evaluateIsOver();
            return true;}
        return false;
    }

}