package DWR.DMS.PTM;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

public class ParticleLoop {
	public static boolean DEBUG = false;
	
	// Get the number of available processors
	public static int NUM_THREADS = Runtime.getRuntime().availableProcessors(); 

	// Create a thread pool executor with a fixed number of threads
	public static ExecutorService executor = Executors.newFixedThreadPool(NUM_THREADS);

	public static void doAll(Particle[] particleArray, int timeStep) {
		int numberOfParticles = particleArray.length;
		int elementsPerThread = (numberOfParticles + NUM_THREADS - 1) / NUM_THREADS;

		List<Callable<Void>> tasks = new ArrayList<>();

		// Create tasks for each chunk of the particle array
		for (int i = 0; i < NUM_THREADS; i++) {
			final int startIndex = i * elementsPerThread;
			final int endIndex = Math.min((i + 1) * elementsPerThread, numberOfParticles);
			tasks.add(() -> {
				for (int j = startIndex; j < endIndex; j++) {
					Particle particle = particleArray[j];
					if (DEBUG)
						System.out.println("Update particle " + j + " position");
					particle.updatePosition(timeStep);
					if (DEBUG)
						System.out.println("Updated particle " + j + " position");
					particle.clear();
				}
				return null;
			});
		}

		try {
			// Invoke all tasks and wait for their completion
			List<Future<Void>> results = executor.invokeAll(tasks);

			// Optionally, you can handle exceptions that occurred during task
			// execution
			for (Future<Void> result : results) {
				result.get(); // This will throw an exception if a task failed
			}
		} catch (InterruptedException | ExecutionException e) {
			e.printStackTrace();
		}

	}
	
	public static void shutdown(){
		executor.shutdown();
	}
}
