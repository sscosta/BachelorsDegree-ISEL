package com.isel.si1.Trabalho.presentation.workers;

import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.SwingWorker;

public class MySimpleWorker<T> extends SwingWorker<T, String> {

	Logger logger = Logger.getLogger(MySimpleWorker.class.getName());
	
	private Callable<T> jobRunnable;
	private Runnable doneRunnable;
	private PostJobRunnable<T> postJobRunnable;

	public MySimpleWorker(Callable<T> theJobRunnable, Runnable theDoneRunnable, PostJobRunnable<T> thePostJobRunnable){
		jobRunnable = theJobRunnable;
		doneRunnable = theDoneRunnable;
		postJobRunnable = thePostJobRunnable;
	}
	
	public MySimpleWorker(Callable<T> theJobRunnable, Runnable theDoneRunnable){
		this(theJobRunnable, theDoneRunnable, (theReturn)->{});
	}

	public MySimpleWorker(Callable<T> theJobRunnable){
		this(theJobRunnable, ()->{}, (theReturn)->{});
	}

	@FunctionalInterface
	public interface PostJobRunnable<V>{
		void run(V theV);
	}
	
	@FunctionalInterface
	public interface SimpleFuncInterface {
	  public void doWork();
	}
	
	@Override
	protected T doInBackground() throws Exception {
		logger.log(Level.FINEST, "Job started.");
		setProgress(1);
		T aReturn = jobRunnable.call();
		setProgress(100);
		logger.log(Level.FINEST, "Job finished.");
		return aReturn;
	}

	@Override
	protected void done() {
		logger.log(Level.FINEST, "called Job done.");
		super.done();
		doneRunnable.run();
		try {
			postJobRunnable.run(get());
		} catch (InterruptedException | ExecutionException theCause) {
			logger.log(Level.SEVERE, "an exception was thrown", theCause);
		}
	}
}